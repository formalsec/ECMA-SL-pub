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

module Parsing' = Parsing
open Prelude
open Debugger_types

module Message = struct
  let help =
    "Commands:\n\
    \  1: eval <expr>\n\
    \  2: locals\n\
    \  3: step [in|out]\n\
    \  4: continue\n\
    \  5: exit\n\
    \  6: help"

  let missing_expr = "Missing expression for evaluation."

  let invalid_expr = "Invalid expression syntax."

  let invalid_step = "Invalid step. Use 'step', 'step in' or 'step out'."

  let invalid = "Unknown command. Try 'help' for more information."
end

module InterpreterCallbacks = struct
  type heapval_pp =
    (Loc.t, unit) Hashtbl.t -> heap -> Format.formatter -> Value.t -> unit

  type eval_expr = state -> Expr.t -> Value.t

  type t =
    { heapval_pp : heapval_pp
    ; eval_expr : eval_expr
    }

  let heapval_pp : heapval_pp ref =
    ref (fun _ _ _ -> Log.fail "debugger value printer not initialized")

  let eval_expr : eval_expr ref =
    ref (fun _ _ -> Log.fail "debugger expression evaluator not initialized")
end

type t =
  | None
  | Print of string
  | Step
  | StepIn
  | StepOut
  | Continue
  | Exit

let heapval_pp (heap : heap) : Value.t Fmt.t =
  let visited = Hashtbl.create !Base.default_hashtbl_sz in
  !InterpreterCallbacks.heapval_pp visited heap

let eval_cmd (state : state) (e_tkns : string list) : string =
  let (_, heap, _) = state in
  if List.is_empty e_tkns then Message.missing_expr
  else
    try
      let e_str = String.concat " " e_tkns in
      let e = Parsing'.parse_expr e_str in
      let v = !InterpreterCallbacks.eval_expr state e in
      Fmt.str "%a" (heapval_pp heap) v
    with _ -> Message.invalid_expr

let locals_cmd (state : state) : string =
  let (store, heap, _) = state in
  let local_f ppf (x, v) =
    if not (String.starts_with ~prefix:"__" x) then
      Fmt.pf ppf "%s: %a\n" x (heapval_pp heap) v
  in
  Fmt.(str "%a" (hashtbl local_f) store)

let step_cmd (step_args : string list) : t =
  match step_args with
  | [] -> Step
  | "in" :: [] -> StepIn
  | "out" :: [] -> StepOut
  | _ -> Print Message.invalid_step

let execute (state : state) (line : string) : t =
  let tkns = String.trim line |> String.split_on_char ' ' in
  match tkns with
  | [] -> Log.fail "invariant : String.split_on_char"
  | "" :: [] -> None
  | "eval" :: e_tkns -> Print (eval_cmd state e_tkns)
  | "locals" :: [] -> Print (locals_cmd state)
  | "step" :: step_tkns -> step_cmd step_tkns
  | "continue" :: [] -> Continue
  | "exit" :: [] -> Exit
  | "help" :: [] -> Print Message.help
  | _ -> Print Message.invalid
