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
open EslBase
open EslSyntax

type arg = Value.t * Source.at

type res = Value.t

let custom_err (at : Source.at) (err : Runtime_error.msg) : 'a =
  Runtime_error.(throw ~src:at err)
[@@inline]

let arg_err (texp : string) ((v, at) : arg) : 'a =
  custom_err at (BadArg (texp, v))
[@@inline]

let mk_bool : bool -> Value.t = function true -> True | false -> False
[@@inline]

let unary_arith_semantics (op : Smtml.Ty.Unop.t) : arg -> res = function
  | ((Int _ as v), _) -> Smtml.Eval.unop Ty_int op v
  | ((Real _ as v), _) -> Smtml.Eval.unop Ty_real op v
  | arg -> arg_err "integer/float" arg

let unary_bitwise_semantics (op : Smtml.Ty.Unop.t) : arg -> res = function
  | ((Int _ as v), _) -> Smtml.Eval.unop Ty_int op v
  | arg -> arg_err "integer" arg

let unary_logical_semantics (op : Smtml.Ty.Unop.t) : arg -> res = function
  | (((True | False) as v), _) -> Smtml.Eval.unop Ty_bool op v
  | arg -> arg_err "boolean" arg

let unary_list_semantics (op : Smtml.Ty.Unop.t) : arg -> res = function
  | ((List _ as v), at) -> (
    try Smtml.Eval.unop Ty_list op v
    with _ -> custom_err at (Unexpected "empty list") )
  | arg -> arg_err "list" arg

let typeof_semantics : arg -> res = function
  | (Int _, _) -> Str "int"
  | (Real _, _) -> Str "float"
  | ((True | False), _) -> Str "bool"
  | (Str _, _) -> Str "string"
  | (List _, _) -> Str "list"
  | (Unit, at) -> custom_err at (Unexpected "void value")
  | (Nothing, _) -> Str "null"
  | (App (`Op "loc", [ Int _ ]), _) -> Str "object"
  | (App (`Op "symbol", [ Str _ ]), _) -> Str "symbol"
  | (App (`Op _, _), _) -> Str "app"
  | (v, _) -> Log.fail "unknown value type: %a" Value.pp v

let int_to_float_semantics : arg -> res = function
  | ((Int _ as v), _) -> Smtml.Eval.cvtop Ty_real Reinterpret_int v
  | arg -> arg_err "integer" arg

let int_to_string_semantics : arg -> res = function
  | ((Int _ as v), _) -> Smtml.Eval.cvtop Ty_str String_from_int v
  | arg -> arg_err "integer" arg

let float_to_int_semantics : arg -> res = function
  | ((Real _ as v), _) -> Smtml.Eval.cvtop Ty_int Reinterpret_float v
  | arg -> arg_err "float" arg

let float_to_string_semantics : arg -> res = function
  | (Real f, _) -> Str (Arith_utils.float_to_string_inner f)
  | arg -> arg_err "float" arg

let string_to_int_semantics : arg -> res = function
  | ((Str _ as v), at) -> (
    try Smtml.Eval.cvtop Ty_str String_to_int v
    with _ -> custom_err at (Custom "Unable to parse string to integer.") )
  | arg -> arg_err "string" arg

let string_to_float_semantics : arg -> res = function
  | ((Str _ as v), _) -> (
    try Smtml.Eval.cvtop Ty_str String_to_float v with _ -> Real nan )
  | arg -> arg_err "string" arg

let binary_plus_semantics : arg * arg -> res = function
  | (((Int _ as v1), _), ((Int _ as v2), _)) ->
    Smtml.Eval.binop Ty_int Add v1 v2
  | (((Real _ as v1), _), ((Real _ as v2), _)) ->
    Smtml.Eval.binop Ty_real Add v1 v2
  | (((Str _ as v1), _), ((Str _ as v2), _)) ->
    Smtml.Eval.naryop Ty_str Concat [ v1; v2 ]
  | ((Int _, _), arg2) -> arg_err "integer" arg2
  | ((Real _, _), arg2) -> arg_err "float" arg2
  | ((Str _, _), arg2) -> arg_err "string" arg2
  | (arg1, _) -> arg_err "integer/float/string" arg1

let binary_arith_semantics (op : Smtml.Ty.Binop.t) : arg * arg -> res = function
  | (((Int _ as v1), _), ((Int _ as v2), _)) -> Smtml.Eval.binop Ty_int op v1 v2
  | (((Real _ as v1), _), ((Real _ as v2), _)) ->
    Smtml.Eval.binop Ty_real op v1 v2
  | ((Int _, _), arg2) -> arg_err "integer" arg2
  | ((Real _, _), arg2) -> arg_err "float" arg2
  | (arg1, _) -> arg_err "integer/float" arg1

let binary_bitwise_semantics (op : Smtml.Ty.Binop.t) : arg * arg -> res =
  function
  | (((Int _ as v1), _), ((Int _ as v2), _)) -> Smtml.Eval.binop Ty_int op v1 v2
  | ((Int _, _), arg2) -> arg_err "integer" arg2
  | (arg1, _) -> arg_err "integer" arg1

let binary_logical_semantics (op : Smtml.Ty.Binop.t) : arg * arg -> res =
  function
  | ((((True | False) as v1), _), (((True | False) as v2), _)) ->
    Smtml.Eval.binop Ty_bool op v1 v2
  | (((True | False), _), arg2) -> arg_err "boolean" arg2
  | (arg1, _) -> arg_err "boolean" arg1

let binary_eq_semantics (op : Smtml.Ty.Relop.t) : arg * arg -> res = function
  | (((Real _ as v1), _), ((Real _ as v2), _)) ->
    (* Reals need special treatment due to nans *)
    mk_bool (Smtml.Eval.relop Ty_real op v1 v2)
  | ((v1, _), (v2, _)) -> mk_bool (Smtml.Eval.relop Ty_bool op v1 v2)

let binary_relation_semantics (op : Smtml.Ty.Relop.t) : arg * arg -> res =
  function
  | (((Int _ as v1), _), ((Int _ as v2), _)) ->
    mk_bool (Smtml.Eval.relop Ty_int op v1 v2)
  | (((Real _ as v1), _), ((Real _ as v2), _)) ->
    mk_bool (Smtml.Eval.relop Ty_real op v1 v2)
  | (((Str _ as v1), _), ((Str _ as v2), _)) ->
    mk_bool (Smtml.Eval.relop Ty_str op v1 v2)
  | ((Int _, _), arg2) -> arg_err "integer" arg2
  | ((Real _, _), arg2) -> arg_err "float" arg2
  | ((Str _, _), arg2) -> arg_err "string" arg2
  | (arg1, _) -> arg_err "integer/float/string" arg1

let conditional_guard_semantics : arg -> res = function
  | (((True | False) as v1), _) -> v1
  | arg1 -> arg_err "boolean" arg1

let conditional_semantics : arg * arg * arg -> res = function
  | ((((True | False) as v1), _), (v2, _), (v3, _)) ->
    Smtml.Eval.triop Ty_bool Ite v1 v2 v3
  | (arg1, _, _) -> arg_err "boolean" arg1

let nary_logical_semantics (op : Smtml.Ty.Binop.t) : arg list -> res =
  let eval_f v1 arg2 = binary_logical_semantics op ((v1, Source.none), arg2) in
  ( match op with
  | And -> True
  | Or -> False
  | _ -> Log.fail "unexpected binary operator in nary logical expression" )
  |> List.fold_left eval_f

let list_expr_semantics : arg list -> res =
 fun args -> List (fst (List.split args))

let unopt_semantics (op : Operator.unopt) : arg -> res =
  match op with
  | Neg -> unary_arith_semantics Neg
  | BitwiseNot -> unary_bitwise_semantics Not
  | LogicalNot -> unary_logical_semantics Not
  | ListHead -> unary_list_semantics Head
  | ListTail -> unary_list_semantics Tail
  | Typeof -> typeof_semantics
  | IntToFloat -> int_to_float_semantics
  | IntToString -> int_to_string_semantics
  | FloatToInt -> float_to_int_semantics
  | FloatToString -> float_to_string_semantics
  | StringToInt -> string_to_int_semantics
  | StringToFloat -> string_to_float_semantics
  | ObjectToList -> Log.fail "unexpected 'ObjectToList' operator evaluation"
  | ObjectFields -> Log.fail "unexpected 'ObjectFields' operator evaluation"

let binopt_semantics (op : Operator.binopt) : arg * arg -> res =
  match op with
  | Plus -> binary_plus_semantics
  | Minus -> binary_arith_semantics Sub
  | Times -> binary_arith_semantics Mul
  | Div -> binary_arith_semantics Div
  | Modulo -> binary_arith_semantics Rem
  | Pow -> binary_arith_semantics Pow
  | BitwiseAnd -> binary_bitwise_semantics And
  | BitwiseOr -> binary_bitwise_semantics Or
  | BitwiseXor -> binary_bitwise_semantics Xor
  | ShiftLeft -> binary_bitwise_semantics Shl
  | ShiftRight -> binary_bitwise_semantics ShrA
  | ShiftRightLogical -> binary_bitwise_semantics ShrL
  | LogicalAnd -> binary_logical_semantics And
  | LogicalOr -> binary_logical_semantics Or
  | SCLogicalAnd -> Log.fail "unexpected 'SCLogicalAnd' operator evaluation"
  | SCLogicalOr -> Log.fail "unexpected 'SCLogicalOr' operator evaluation"
  | Eq -> binary_eq_semantics Eq
  | Ne -> binary_eq_semantics Ne
  | Lt -> binary_relation_semantics Lt
  | Gt -> binary_relation_semantics Gt
  | Le -> binary_relation_semantics Le
  | Ge -> binary_relation_semantics Ge
  | ObjectMem -> Log.fail "unexpected 'ObjectMem' operator evaluation"

let triopt_semantics (op : Operator.triopt) : arg * arg * arg -> res =
  match op with Conditional -> conditional_semantics

let nopt_semantics (op : Operator.nopt) : arg list -> res =
  match op with
  | NAryLogicalAnd -> nary_logical_semantics And
  | NAryLogicalOr -> nary_logical_semantics Or
  | ListExpr -> list_expr_semantics
