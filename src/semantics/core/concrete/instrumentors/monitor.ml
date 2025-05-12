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

open EslSyntax

type stmt_eval =
  | SkipEval
  | MergeEval
  | DebugEval
  | BlockEval
  | PrintEval
  | ReturnEval
  | AssignEval
  | AssignCallEval of Func.t
  | AssignECallEval
  | AssignNewObjEval of Loc.t
  | AssignObjToListEval
  | AssignObjFieldsEval
  | AssignInObjCheckEval of Loc.t * string
  | FieldLookupEval of Loc.t * string
  | FieldAssignEval of Loc.t * string
  | FieldDeleteEval of Loc.t * string
  | IfEval of bool
  | WhileEval
  | SwitchEval of Value.t
  | FailEval
  | AssertEval of bool
  | AbortEval

module type M = sig
  type sl

  type state

  type return

  type 'a label

  type t =
    { mutable state : state
    ; mutable label : sl label
    }

  val initial_state : unit -> t

  val set_label : t -> sl label -> unit

  val update_label : t -> Stmt.t -> stmt_eval -> unit

  val eval_small_step : t -> unit

  val interceptor : string -> Value.t list -> Expr.t list -> sl label option
end

module Default : M = struct
  type sl = unit

  type state = unit

  type return = unit

  type 'a label = unit

  type t =
    { mutable state : state
    ; mutable label : sl label
    }

  let initial_state () : t = { state = (); label = () }

  let set_label (mon : t) (label : sl label) : unit = mon.label <- label

  let update_label (_ : t) (_ : Stmt.t) (_ : stmt_eval) : unit = ()

  let eval_small_step (_ : t) : unit = ()

  let interceptor (_ : string) (_ : Value.t list) (_ : Expr.t list) :
    sl label option =
    None
end
