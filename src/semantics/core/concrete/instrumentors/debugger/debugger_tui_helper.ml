(* Copyright (C) 2022-2025 formalsec programmers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

open Prelude
include Curses
open EslBase

module Config = struct
  let default_scrollable_sz = 1000
end

let ( !! ) (sucess : bool) : unit =
  if not sucess then (
    endwin ();
    Log.fail "error in the debugger TUI" )

let proportional_sz ?(remainder : bool = false) (total : int) (factor : int)
  (weight : int) : int =
  let totalf = float_of_int total in
  let factorf = float_of_int factor in
  let weightf = float_of_int weight in
  let psz = weightf *. totalf /. factorf in
  int_of_float (if remainder then ceil psz else floor psz)

let draw_rectangle (w : window) (y : int) (x : int) (yz : int) (xz : int)
  (fill : char) : unit =
  let range (f : int) (l : int) = List.init (l - f) (fun i -> f + i) in
  let row fill' y' = mvwhline w y' x fill' xz in
  List.iter (row (Char.code fill)) (range y (y + yz))

type 'a element =
  { v : 'a
  ; window : 'a -> window
  ; refresh : 'a -> unit
  ; element : 'a -> 'a element
  }

module Win = struct
  type t =
    { w : window
    ; y : int
    ; x : int
    ; yz : int
    ; xz : int
    }

  let mk (parentwin : t) (y : int) (x : int) (yz : int) (xz : int) : t =
    let w = derwin parentwin.w yz xz y x in
    { w; y; x; yz; xz }

  let window (win : t) : window = win.w

  let refresh (win : t) : unit = !!(wrefresh win.w)

  let rec element (win : t) : t element = { v = win; window; refresh; element }
end

module ScrollPad = struct
  type t =
    { w : window
    ; rows : int
    ; first : int
    ; y : int
    ; x : int
    ; yz : int
    ; xz : int
    }

  let mk ?(rows : int option) (y : int) (x : int) (yz : int) (xz : int) : t =
    let rows' = Option.value ~default:Config.default_scrollable_sz rows in
    if rows' < yz then Log.fail "expecting rows > yz";
    let w = newpad rows' xz in
    scrollok w true;
    { w; rows = rows'; first = rows' - yz; y; x; yz; xz }

  let window (scrlpad : t) : window = scrlpad.w

  let refresh (scrlpad : t) : unit =
    !!(prefresh scrlpad.w scrlpad.first 0 scrlpad.y scrlpad.x
         (scrlpad.y + scrlpad.yz) (scrlpad.x + scrlpad.xz) )

  let rec element (scrlpad : t) : t element =
    { v = scrlpad; window; refresh; element }
end

module Frame = struct
  type border =
    { t : chtype
    ; l : chtype
    ; b : chtype
    ; r : chtype
    ; tl : chtype
    ; tr : chtype
    ; bl : chtype
    ; br : chtype
    }

  let dflt_border (acs : Acs.acs) : border =
    { t = acs.hline
    ; l = acs.vline
    ; b = acs.hline
    ; r = acs.vline
    ; tl = acs.ulcorner
    ; tr = acs.urcorner
    ; bl = acs.llcorner
    ; br = acs.lrcorner
    }

  type 'a t =
    { border : border
    ; framewin : Win.t
    ; el : 'a element
    }

  let mk ?(border : border option) (acs : Acs.acs) (framewin : Win.t)
    (el : 'a element) : 'a t =
    let border = Option.value ~default:(dflt_border acs) border in
    { border; framewin; el }

  let window (frame : 'a t) : window = frame.framewin.w

  let refresh (frame : 'a t) : unit =
    !!(wrefresh frame.framewin.w);
    frame.el.refresh frame.el.v

  let rec element (frame : 'a t) : 'a t element =
    { v = frame; window; refresh; element }

  let resize (frame : 'a t) (framewin : Win.t) (el : 'a element) : 'a t =
    { frame with framewin; el }

  let draw (frame : 'a t) : unit =
    let b = frame.border in
    wborder frame.framewin.w b.l b.r b.t b.b b.tl b.tr b.bl b.br
end

module Prompt = struct
  type t =
    { scrlpad : ScrollPad.t
    ; row : int
    ; col : int
    ; text : string
    ; len : int
    ; pos : int
    ; y : int
    ; x : int
    }

  let mk (scrlpad : ScrollPad.t) (row : int) (col : int) : t =
    { scrlpad; text = ""; len = 0; pos = 0; row; col; y = row; x = col }

  let window (prompt : t) : window = ScrollPad.window prompt.scrlpad

  let refresh (prompt : t) : unit = ScrollPad.refresh prompt.scrlpad

  let rec element (prompt : t) : t element =
    { v = prompt; window; refresh; element }

  let update (prompt : t) (text : string) (len : int) (pos : int) : t =
    let width = prompt.scrlpad.xz in
    let shifted_len = prompt.col + len in
    let shifted_pos = prompt.col + pos in
    let row_shift = ((shifted_len - 1) / width) - (shifted_pos / width) in
    let y = prompt.row - max 0 row_shift in
    let x = shifted_pos mod width in
    { prompt with text; len; pos; y; x }

  let scrldown_fill (prompt : t) (shift : int) : unit =
    draw_rectangle prompt.scrlpad.w prompt.row shift 1 prompt.scrlpad.xz ' '

  let scrldown (prompt : t) : unit =
    !!(wscrl prompt.scrlpad.w 1);
    scrldown_fill prompt 0

  let scrlup (prompt : t) : unit = !!(wscrl prompt.scrlpad.w (-1))

  let reset (prompt : t) : t * string =
    let prompt' = mk prompt.scrlpad prompt.row prompt.col in
    (prompt', prompt.text)

  let move_left (prompt : t) : t =
    let pos = max (prompt.pos - 1) 0 in
    let prompt' = update prompt prompt.text prompt.len pos in
    if prompt.x = 0 && prompt.pos = prompt.len then scrlup prompt';
    !!(wmove prompt'.scrlpad.w prompt'.y prompt'.x);
    prompt'

  let move_right (prompt : t) : t =
    let pos = min (prompt.pos + 1) prompt.len in
    let prompt' = update prompt prompt.text prompt.len pos in
    if (not (prompt.x = 0)) && prompt'.x = 0 && prompt'.pos = prompt'.len then
      scrldown prompt';
    !!(wmove prompt'.scrlpad.w prompt'.y prompt'.x);
    prompt'

  let add (prompt : t) (input : int) : t =
    let (left, right) = String.split_at_index prompt.pos prompt.text in
    let text = Fmt.str "%s%c%s" left (Char.chr input) right in
    !!(waddch prompt.scrlpad.w input);
    !!(waddstr prompt.scrlpad.w right);
    let prompt' = update prompt text (prompt.len + 1) (prompt.pos + 1) in
    let (_, curs_x) = getyx prompt'.scrlpad.w in
    if curs_x = 0 && prompt.pos = prompt.len then scrldown_fill prompt' 0
    else if curs_x = 0 then scrlup prompt'
    else if curs_x = 1 then scrldown_fill prompt' 1;
    !!(wmove prompt'.scrlpad.w prompt'.y prompt'.x);
    prompt'

  let delete (prompt : t) : t =
    if prompt.pos = String.length prompt.text then prompt
    else
      let (left, right) = String.split_at_index prompt.pos prompt.text in
      let right' = String.substr ~left:1 right in
      let text = Fmt.str "%s%s" left right' in
      !!(waddstr prompt.scrlpad.w right');
      !!(waddch prompt.scrlpad.w (Char.code ' '));
      let prompt' = update prompt text (prompt.len - 1) prompt.pos in
      let (_, curs_x) = getyx prompt'.scrlpad.w in
      if curs_x = 1 && prompt'.pos = prompt'.len then ()
      else if curs_x = 1 then scrlup prompt'
      else if curs_x = 0 then scrlup prompt';
      !!(wmove prompt'.scrlpad.w prompt'.y prompt'.x);
      prompt'

  let backspace (prompt : t) : t =
    if prompt.pos = 0 then prompt else move_left prompt |> delete

  let enter (prompt : t) : t =
    scrldown prompt;
    let (prompt', _) = reset prompt in
    !!(wmove prompt'.scrlpad.w prompt'.y prompt'.x);
    prompt'

  let process (prompt : t) (input : int) : t =
    if input >= 32 && input <= 126 then add prompt input
    else if input = Key.dc then delete prompt
    else if input = Key.backspace then backspace prompt
    else if input = Key.left then move_left prompt
    else if input = Key.right then move_right prompt
    else if input = Key.enter || input = 10 then enter prompt
    else prompt
end

module Interface = struct
  type 'a t =
    { active : bool
    ; callback : 'a -> int -> 'a
    ; el : 'a element
    }

  let input () : int =
    let input = getch () in
    flushinp ();
    input

  let mk ?(active : bool = false) (callback : 'a -> int -> 'a) (el : 'a element)
    : 'a t =
    { active; callback; el }

  let window (intf : 'a t) : window = intf.el.window intf.el.v

  let refresh (intf : 'a t) : unit = intf.el.refresh intf.el.v

  let rec element (intf : 'a t) : 'a t element =
    { v = intf; window; refresh; element }

  let update (intf : 'a t) (input : int) : 'a t =
    if intf.active then
      { intf with el = intf.el.element (intf.callback intf.el.v input) }
    else intf
end

module Color = struct
  include Color

  type t = int

  let (next, _) = Base.make_counter 1 1

  let none = 0

  let mk (fg : int) (bg : int) : int =
    if not (has_colors ()) then 0
    else
      let id = next () in
      !!(init_pair id fg bg);
      id
end
