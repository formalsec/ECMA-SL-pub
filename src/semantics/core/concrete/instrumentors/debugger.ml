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
module DebuggerTUI = Debugger_tui
module InterpreterCallbacks = Debugger_cmd.InterpreterCallbacks

module BreakpointInjector = struct
  let inject_debug_none (stack : stack) (cont : cont) : stack * cont =
    (stack, cont)

  let rec inject_debug_innerscope (stack : stack) (cont : cont) : stack * cont =
    match cont with
    | { it; at } :: cont' when not at.real ->
      let (stack'', cont'') = inject_debug_innerscope stack cont' in
      (stack'', { it; at } :: cont'')
    | { it = Debug _; _ } :: _ -> (stack, cont)
    | { it = Block ss; at } :: cont' ->
      let (stack'', cont'') = inject_debug_innerscope stack ss in
      (stack'', { it = Block cont''; at } :: cont')
    | s :: cont' -> (stack, Source.(Stmt.Debug s @> s.at) :: cont')
    | [] -> inject_debug_outerscope stack cont

  and inject_debug_outerscope (stack : stack) (cont : cont) : stack * cont =
    let open Call_stack in
    match pop stack with
    | (Toplevel _, _) -> (stack, cont)
    | (Intermediate (cursor, restore), stack') ->
      let { f; _ } = cursor in
      let { store; cont = cont'; retvar } = restore in
      let (stack'', cont'') = inject_debug_innerscope stack' cont' in
      (push stack'' f store cont'' retvar, cont)
end

module type M = sig
  type t

  val initial_state : unit -> t

  val cleanup : t -> unit

  val set_interp_callbacks : InterpreterCallbacks.t -> unit

  val run : t -> Code_utils.t -> state -> cont -> Stmt.t -> state * cont

  val call : t -> stack -> cont -> stack * cont
end

module Disable : M = struct
  type t = unit

  let initial_state () : t = ()

  let cleanup (_ : t) : unit = ()

  let set_interp_callbacks (_ : InterpreterCallbacks.t) : unit = ()

  let run (_ : t) _ (st : state) (cont : cont) (_ : Stmt.t) : state * cont =
    (st, cont)

  let call (_ : t) (stack : stack) (cont : cont) : stack * cont = (stack, cont)
end

module Enable : M = struct
  type dbdata =
    { streams : Log.Redirect.t
    ; tui : DebuggerTUI.t
    }

  type t' =
    | Initial
    | StmtBreak of dbdata
    | CallBreak of dbdata
    | Final

  type t = t' ref

  let initialize_debug_tui () : dbdata =
    let streams = Log.Redirect.capture Shared in
    try
      let tui = DebuggerTUI.initialize () in
      DebuggerTUI.render_static tui;
      { streams; tui }
    with exn ->
      Log.Redirect.restore ~log:true streams;
      raise exn

  let terminate_debug_tui (data : dbdata) : unit =
    DebuggerTUI.terminate ();
    Log.Redirect.restore ~log:true data.streams

  let run_debug_tui code (st : state) (s : Stmt.t) (dbdata : dbdata) : dbdata =
    let rec run_debug_tui_loop tui =
      let tui' = DebuggerTUI.update code tui in
      if tui'.running then run_debug_tui_loop tui' else tui'
    in
    let tui = DebuggerTUI.set_data dbdata.tui st s in
    DebuggerTUI.render code tui;
    let tui' = run_debug_tui_loop tui in
    { dbdata with tui = tui' }

  let initial_state () : t = ref Initial

  let cleanup (db : t) : unit =
    match !db with
    | Initial -> ()
    | StmtBreak db' -> terminate_debug_tui db'
    | CallBreak db' -> terminate_debug_tui db'
    | Final -> ()

  let set_interp_callbacks (interp_callbacks : InterpreterCallbacks.t) : unit =
    InterpreterCallbacks.heapval_pp := interp_callbacks.heapval_pp;
    InterpreterCallbacks.eval_expr := interp_callbacks.eval_expr

  let next_state (db : t) (st : state) (cont : cont) (s : Stmt.t)
    (dbdata : dbdata) : state * cont =
    let ( <@ ) inject_f (db', st, cont) =
      let (store, heap, stack) = st in
      let (stack', cont') = inject_f stack cont in
      db := db';
      ((store, heap, stack'), cont')
    in
    let none = BreakpointInjector.inject_debug_none in
    let inerscope = BreakpointInjector.inject_debug_innerscope in
    let outerscope = BreakpointInjector.inject_debug_outerscope in
    let terminate () = terminate_debug_tui dbdata |> fun () -> none in
    match (DebuggerTUI.get_last_cmd dbdata.tui, s) with
    | (Step, _) -> inerscope <@ (StmtBreak dbdata, st, cont)
    | (StepIn, { it = AssignCall _; _ }) -> none <@ (CallBreak dbdata, st, cont)
    | (StepIn, _) -> inerscope <@ (StmtBreak dbdata, st, cont)
    | (StepOut, _) -> outerscope <@ (StmtBreak dbdata, st, cont)
    | (Continue, _) -> none <@ (StmtBreak dbdata, st, cont)
    | (Exit, _) -> terminate () <@ (Final, st, cont)
    | _ -> Log.fail "expecting debugger flow action"

  let run (db : t) code (st : state) (cont : cont) (s : Stmt.t) : state * cont =
    let run_debug_tui' = run_debug_tui code st s in
    let next_state' = next_state db st cont s in
    match !db with
    | Initial -> initialize_debug_tui () |> run_debug_tui' |> next_state'
    | StmtBreak tui -> run_debug_tui' tui |> next_state'
    | CallBreak tui -> run_debug_tui' tui |> next_state'
    | Final -> (st, cont)

  let call (db : t) (stack : stack) (cont : cont) : stack * cont =
    let open BreakpointInjector in
    match !db with
    | CallBreak dbdata ->
      let (stack', cont') = inject_debug_innerscope stack cont in
      db := StmtBreak dbdata;
      (stack', cont')
    | _ -> (stack, cont)
end
