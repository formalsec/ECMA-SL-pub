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

open EslBase
open EslSyntax

let ( ~@ ) (t : EType.t') : EType.t = Source.(t @?> none)

type op_signature = (EType.t' list * EType.t') list

let type_check_signature (targs : EType.t list) (tpxs : EType.t' list) : unit =
  let type_check_operand (tpx, targ) =
    try TSubtyping.type_check ~@tpx targ
    with Typing_error.Error err ->
      Typing_error.(update (BadOperand (~@tpx, targ)) err |> raise)
  in
  List.combine tpxs targs |> List.iter type_check_operand

let rec type_operator_strict ?(err : Typing_error.t option = None)
  (targs : EType.t list) (op_sig : op_signature) : EType.t' =
  match (op_sig, err) with
  | ([], None) -> Log.fail "expecting type error"
  | ([], Some err') -> Typing_error.raise err'
  | ((tpxs, tret) :: op_sig', _) -> (
    try type_check_signature targs tpxs |> fun () -> tret
    with Typing_error.Error err' ->
      let err = Some (Option.value ~default:err' err) in
      type_operator_strict ~err targs op_sig' )

let type_operator (targs : EType.t list) (op_sig : op_signature) : EType.t' =
  let has_tany_f = EType.equal ~@AnyType in
  if List.length op_sig > 1 && List.exists has_tany_f targs then AnyType
  else type_operator_strict targs op_sig

let type_unopt (op : Operator.unopt) (targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  let type_op = type_operator targs in
  match op with
  | Neg -> type_op [ ([ IntType ], IntType); ([ FloatType ], FloatType) ]
  | BitwiseNot -> type_op [ ([ FloatType ], FloatType) ]
  | LogicalNot -> type_op [ ([ BooleanType ], BooleanType) ]
  | IntToFloat -> type_op [ ([ IntType ], FloatType) ]
  | IntToString -> type_op [ ([ IntType ], StringType) ]
  | FloatToInt -> type_op [ ([ FloatType ], IntType) ]
  | FloatToString -> type_op [ ([ FloatType ], StringType) ]
  | StringToInt -> type_op [ ([ StringType ], IntType) ]
  | StringToFloat -> type_op [ ([ StringType ], FloatType) ]
  | Typeof -> not_implemented (* TODO *)
  | ObjectToList -> not_implemented (* TODO: custom object typing function *)
  | ObjectFields -> not_implemented (* TODO: custom object typing function *)
  | ListHead -> not_implemented (* TODO: list typing *)
  | ListTail -> not_implemented (* TODO: list typing *)

let type_binopt (op : Operator.binopt) (targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  let type_op = type_operator targs in
  let int_arith = EType.([ IntType; IntType ], IntType) in
  let float_arith = EType.([ FloatType; FloatType ], FloatType) in
  match op with
  | Plus ->
    type_op [ int_arith; float_arith; ([ StringType; StringType ], StringType) ]
  | Minus -> type_op [ int_arith; float_arith ]
  | Times -> type_op [ int_arith; float_arith ]
  | Div -> type_op [ int_arith; float_arith ]
  | Modulo -> type_op [ float_arith ]
  | Pow -> type_op [ float_arith ]
  | BitwiseAnd -> type_op [ int_arith ]
  | BitwiseOr -> type_op [ int_arith ]
  | BitwiseXor -> type_op [ int_arith ]
  | ShiftLeft -> type_op [ int_arith ]
  | ShiftRight -> type_op [ int_arith ]
  | ShiftRightLogical -> type_op [ int_arith ]
  | LogicalAnd -> type_op [ ([ BooleanType; BooleanType ], BooleanType) ]
  | LogicalOr -> type_op [ ([ BooleanType; BooleanType ], BooleanType) ]
  | SCLogicalAnd -> type_op [ ([ BooleanType; BooleanType ], BooleanType) ]
  | SCLogicalOr -> type_op [ ([ BooleanType; BooleanType ], BooleanType) ]
  | Eq | Ne -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | Lt -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | Gt -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | Le -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | Ge -> type_op [ ([ UnknownType; UnknownType ], BooleanType) ]
  | ObjectMem -> not_implemented (* TODO: custom object typing function *)

let type_triopt (op : Operator.triopt) (_targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  match op with
  | Conditional -> not_implemented (* TODO: conditional typing function *)

let type_nopt (op : Operator.nopt) (_targs : EType.t list) : EType.t' =
  let not_implemented = EType.AnyType in
  match op with
  | NAryLogicalAnd -> not_implemented (* TODO: nopt typing *)
  | NAryLogicalOr -> not_implemented (* TODO: nopt typing *)
  | ListExpr -> not_implemented (* TODO: nopt typing *)
