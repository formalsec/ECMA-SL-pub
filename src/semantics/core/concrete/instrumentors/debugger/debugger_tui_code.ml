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
open Debugger_types
open Debugger_tui_helper

type colors =
  { title : Color.t
  ; location : Color.t
  ; code_off : Color.t
  ; code_on : Color.t
  }

type t =
  { frame : Win.t Frame.t
  ; colors : colors
  ; at : Source.at
  }

let colors () : colors =
  { title = Color.mk Color.cyan Color.white
  ; location = Color.mk Color.white Color.white
  ; code_off = Color.mk Color.white Color.black
  ; code_on = Color.mk Color.cyan Color.black
  }

let create (acs : Acs.acs) (consolewin : Win.t) : t =
  let (y, x) = (0, 0) in
  let yz = proportional_sz consolewin.yz 5 3 in
  let xz = proportional_sz consolewin.xz 3 2 in
  let framewin = Win.mk consolewin y x yz xz in
  let win = Win.mk framewin 1 1 (yz - 2) (xz - 2) in
  let frame = Frame.mk acs framewin (Win.element win) in
  let colors = colors () in
  { frame; colors; at = Source.none }

let resize (code : t) (consolewin : Win.t) : t =
  let (y, x) = (0, 0) in
  let yz = proportional_sz consolewin.yz 5 3 in
  let xz = proportional_sz consolewin.xz 3 2 in
  let framewin = Win.mk consolewin y x yz xz in
  let win = Win.mk framewin 1 1 (yz - 2) (xz - 2) in
  let frame = Frame.resize code.frame framewin (Win.element win) in
  { code with frame }

let window (code : t) : window = Frame.window code.frame

let refresh (code : t) : unit = Frame.refresh code.frame

let rec element (code : t) : t element = { v = code; window; refresh; element }

let set_data (code : t) (at : Source.at) : t = { code with at }

let render_static (code : t) : unit = Frame.draw code.frame

let codeblock (nlines : int) ((code, at) : Code_utils.t * Source.at) :
  (int * string) list * int =
  let file = Code_utils.file code at.file in
  let file_sz = Code_utils.file_sz file in
  let line = at.lpos.line in
  let prev_nlines = proportional_sz nlines 3 1 in
  let codeblock_start = max (line - prev_nlines) 1 in
  let codeblock_sz = min nlines (file_sz - codeblock_start + 1) in
  let codeblock = Code_utils.lines file codeblock_start codeblock_sz in
  let last_line = codeblock_start + codeblock_sz in
  (codeblock, last_line)

let render_codeline (win : Win.t) (lineno_sz : int) (i : int)
  ((lineno, line, color) : int * string * Color.t) : unit =
  let lineno' = Fmt.str "%*d |" (15 + lineno_sz) lineno in
  let line' = Re.(global_replace (regexp "\t") "  " line) in
  let (line'', trunc) = String.truncate (win.xz - 19 - lineno_sz) line' in
  wattr_set win.w WA.normal color;
  !!(mvwaddstr win.w i (18 + lineno_sz) line'');
  wattr_set win.w WA.dim color;
  !!(mvwaddstr win.w i 0 lineno');
  if trunc then !!(mvwaddstr win.w i (win.xz - 3) "...")

let render_code code (win : Win.t) (colors : colors) (at : Source.at) : unit =
  let color l = if l = at.lpos.line then colors.code_on else colors.code_off in
  let colors_f (lineno, line) = (lineno, line, color lineno) in
  let (codeblock, last_line) = codeblock (win.yz - 2) (code, at) in
  let lineno_sz = String.length (string_of_int last_line) in
  let codeblock_colored = List.map colors_f codeblock in
  List.iteri (render_codeline win lineno_sz) codeblock_colored

let render_loc_data (win : Win.t) (at : Source.at) : unit =
  let lineno_sz = String.length (string_of_int at.lpos.line) in
  let (file', trunc) = String.truncate (win.xz - 19 - lineno_sz) at.file in
  let file'' = if trunc then String.cat file' "..." else file' in
  let loc = Fmt.str "File %S, line %d" file'' at.lpos.line in
  !!(mvwaddstr win.w (win.yz - 1) 1 loc)

let render_loc (win : Win.t) (colors : colors) (at : Source.at) : unit =
  wattr_set win.w WA.(combine [ dim; standout ]) colors.location;
  draw_rectangle win.w (win.yz - 1) 0 1 win.xz ' ';
  render_loc_data win at

let render_title (win : Win.t) (colors : colors) : unit =
  wattr_set win.w WA.(combine [ dim; standout ]) colors.title;
  draw_rectangle win.w 0 0 4 15 ' ';
  !!(mvwaddstr win.w 1 4 "ECMA-SL");
  !!(mvwaddstr win.w 2 3 "Debugging")

let render code_tbl (code : t) : unit =
  werase code.frame.el.v.w;
  render_code code_tbl code.frame.el.v code.colors code.at;
  render_loc code.frame.el.v code.colors code.at;
  render_title code.frame.framewin code.colors
