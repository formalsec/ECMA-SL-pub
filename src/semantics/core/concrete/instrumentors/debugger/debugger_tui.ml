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
module Code = Debugger_tui_code
module View = Debugger_tui_view
module Execution = Debugger_tui_execution
module Terminal = Debugger_tui_terminal
module Command = Debugger_cmd

type t =
  { acs : Acs.acs
  ; consolewin : Win.t
  ; code : Code.t
  ; view : View.t
  ; exec : Execution.t
  ; term : Terminal.t Interface.t
  ; running : bool
  }

let test_term_size (yz : int) (xz : int) : unit =
  let open EslBase in
  (* FIXME *)
  if yz < 1 || xz < 40 then (
    endwin ();
    Log.fail "expecting larger terminal size" )

let initialize () : t =
  let open Interface in
  try
    let w = initscr () in
    let (yz, xz) = get_size () in
    let consolewin = Win.{ w; y = 0; x = 0; yz; xz } in
    test_term_size yz xz;
    !!(cbreak ());
    !!(noecho ());
    !!(curs_set 1);
    !!(intrflush consolewin.w false);
    !!(keypad consolewin.w true);
    !!(start_color ());
    !!(use_default_colors ());
    nl ();
    let acs = get_acs_codes () in
    let code = Code.create acs consolewin in
    let view = View.create acs consolewin code.frame.framewin in
    let exec = Execution.create acs consolewin code.frame.framewin in
    let term' = Terminal.create consolewin exec.frame.framewin in
    let term = Terminal.(mk ~active:true callback (element term')) in
    { acs; consolewin; code; view; exec; term; running = true }
  with exn -> endwin () |> fun () -> raise exn

let resize (tui : t) : t =
  let open Interface in
  let w = initscr () in
  let (yz, xz) = get_size () in
  let consolewin = Win.{ w; y = 0; x = 0; yz; xz } in
  let code = Code.resize tui.code consolewin in
  let view = View.resize tui.view consolewin code.frame.framewin in
  let exec = Execution.resize tui.exec consolewin code.frame.framewin in
  let term' = Terminal.resize tui.term.el.v consolewin exec.frame.framewin in
  let term = Terminal.(mk ~active:true callback (element term')) in
  { tui with consolewin; code; view; exec; term }

let terminate () : unit = endwin ()

let window (tui : t) : window = tui.consolewin.w

let refresh (tui : t) : unit =
  Code.refresh tui.code;
  View.refresh tui.view;
  Execution.refresh tui.exec;
  Interface.refresh tui.term;
  !!(refresh ())

let rec element (tui : t) : t element = { v = tui; window; refresh; element }

let set_data (tui : t) (st : state) (s : Stmt.t) : t =
  let code = Code.set_data tui.code s.at in
  let term' = Terminal.set_data tui.term.el.v st in
  let term = { tui.term with el = Terminal.element term' } in
  { tui with code; term }

let get_last_cmd (tui : t) : Command.t = Terminal.get_last_cmd tui.term.el.v

let render_static (tui : t) : unit =
  Code.render_static tui.code;
  View.render_static tui.view;
  Execution.render_static tui.exec;
  Terminal.render_static tui.term.el.v;
  refresh tui

let render code (tui : t) : unit =
  Code.render code tui.code;
  refresh tui

let resize_cmd code (tui : t) : t =
  flushinp ();
  erase ();
  let tui' = resize tui in
  render_static tui';
  render code tui';
  refresh tui';
  tui'

let update_running (tui : t) : t =
  let open Command in
  let running = function None | Print _ -> true | _ -> false in
  { tui with running = running tui.term.el.v.last_cmd }

let update code (tui : t) : t =
  let input = Interface.input () in
  if input = Key.resize then resize_cmd code tui
  else
    let term = Interface.update tui.term input in
    let tui' = { tui with term } in
    update_running tui'
