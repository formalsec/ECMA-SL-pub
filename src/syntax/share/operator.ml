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

type unopt =
  | Neg
  | BitwiseNot
  | LogicalNot
  | ListHead
  | ListTail
  | Typeof
  | IntToFloat
  | IntToString
  | FloatToInt
  | FloatToString
  | StringToInt
  | StringToFloat
  | ObjectToList
  | ObjectFields

type binopt =
  | Plus
  | Minus
  | Times
  | Div
  | Modulo
  | Pow
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ShiftRightLogical
  | LogicalAnd
  | LogicalOr
  | SCLogicalAnd
  | SCLogicalOr
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | ObjectMem

type triopt = Conditional

type nopt =
  | ListExpr
  | NAryLogicalAnd
  | NAryLogicalOr

let unopt_label (op : unopt) : string =
  match op with
  | Neg -> "Arith.neg (-)"
  | BitwiseNot -> "Bitwise.not (~)"
  | LogicalNot -> "Logical.not (!)"
  | ListHead -> "List.head (hd)"
  | ListTail -> "List.tail (tl)"
  | Typeof -> "Type.of (typeof)"
  | IntToFloat -> "Type.intToFloat (int_to_float)"
  | IntToString -> "Type.intToString (int_to_string)"
  | FloatToInt -> "Type.floatToInt (float_to_int)"
  | FloatToString -> "Type.floatToString (float_to_string)"
  | StringToInt -> "Type.stringToInt (string_to_int)"
  | StringToFloat -> "Type.stringToFloat (string_to_float)"
  | ObjectToList -> "Object.toList (obj_to_list)"
  | ObjectFields -> "Object.fields (obj_fields)"

let binopt_label (op : binopt) : string =
  match op with
  | Plus -> "Arith.plus (+)"
  | Minus -> "Arith.minus (-)"
  | Times -> "Arith.times (*)"
  | Div -> "Arith.div (/)"
  | Modulo -> "Arith.mod (%)"
  | Pow -> "Arith.pow (**)"
  | BitwiseAnd -> "Bitwise.and (&)"
  | BitwiseOr -> "Bitwise.or (|)"
  | BitwiseXor -> "Bitwise.xor (^)"
  | ShiftLeft -> "Bitwise.shl (<<)"
  | ShiftRight -> "Bitwise.shrA (>>)"
  | ShiftRightLogical -> "Bitwise.shrL (>>>)"
  | LogicalAnd -> "Logical.and (&&)"
  | LogicalOr -> "Logical.or (||)"
  | SCLogicalAnd -> "Logical.scAnd (&&&)"
  | SCLogicalOr -> "Logical.scOr (|||)"
  | Eq -> "Relation.eq (==)"
  | Ne -> "Relation.ne (!=)"
  | Lt -> "Relation.lt (<)"
  | Gt -> "Relation.gt (>)"
  | Le -> "Relation.le (<=)"
  | Ge -> "Relation.ge (>=)"
  | ObjectMem -> "Object.in (in_obj)"

let triopt_label (op : triopt) : string =
  match op with Conditional -> "Relation.conditional (?)"

let nopt_label (op : nopt) : string =
  match op with
  | ListExpr -> "List.expr ([...])"
  | NAryLogicalAnd -> "Logical.nAnd (&&...)"
  | NAryLogicalOr -> "Logical.nOr (||...)"

let unopt_pp_simple : unopt Fmt.t =
 fun ppf -> function
  | Neg -> Fmt.string ppf "-"
  | BitwiseNot -> Fmt.string ppf "~"
  | LogicalNot -> Fmt.string ppf "!"
  | ListHead -> Fmt.string ppf "hd"
  | ListTail -> Fmt.string ppf "tl"
  | Typeof -> Fmt.string ppf "typeof"
  | IntToFloat -> Fmt.string ppf "int_to_float"
  | IntToString -> Fmt.string ppf "int_to_string"
  | FloatToInt -> Fmt.string ppf "float_to_int"
  | FloatToString -> Fmt.string ppf "float_to_string"
  | StringToInt -> Fmt.string ppf "string_to_int"
  | StringToFloat -> Fmt.string ppf "string_to_float"
  | ObjectToList -> Fmt.string ppf "obj_to_list"
  | ObjectFields -> Fmt.string ppf "obj_fields"

let binopt_pp_simple : binopt Fmt.t =
 fun ppf -> function
  | Plus -> Fmt.string ppf "+"
  | Minus -> Fmt.string ppf "-"
  | Times -> Fmt.string ppf "*"
  | Div -> Fmt.string ppf "/"
  | Modulo -> Fmt.string ppf "%"
  | Pow -> Fmt.string ppf "**"
  | BitwiseAnd -> Fmt.string ppf "&"
  | BitwiseOr -> Fmt.string ppf "|"
  | BitwiseXor -> Fmt.string ppf "^"
  | ShiftLeft -> Fmt.string ppf "<<"
  | ShiftRight -> Fmt.string ppf ">>"
  | ShiftRightLogical -> Fmt.string ppf ">>>"
  | LogicalAnd -> Fmt.string ppf "&&"
  | LogicalOr -> Fmt.string ppf "||"
  | SCLogicalAnd -> Fmt.string ppf "&&&"
  | SCLogicalOr -> Fmt.string ppf "|||"
  | Eq -> Fmt.string ppf "=="
  | Ne -> Fmt.string ppf "!="
  | Lt -> Fmt.string ppf "<"
  | Gt -> Fmt.string ppf ">"
  | Le -> Fmt.string ppf "<="
  | Ge -> Fmt.string ppf ">="
  | ObjectMem -> Fmt.string ppf "in_obj"

let triopt_pp_simple : triopt Fmt.t =
 fun ppf -> function Conditional -> Fmt.string ppf "?"

let nopt_pp_simple : nopt Fmt.t =
 fun ppf -> function
  | ListExpr -> Fmt.string ppf "[...]"
  | NAryLogicalAnd -> Fmt.string ppf "&&..."
  | NAryLogicalOr -> Fmt.string ppf "||..."

let unopt_pp ~(pp_v : 'a Fmt.t) (ppf : Format.formatter) ((op, v) : unopt * 'a)
  : unit =
  match op with
  | Neg -> Fmt.pf ppf "%a(%a)" unopt_pp_simple op pp_v v
  | BitwiseNot -> Fmt.pf ppf "%a%a" unopt_pp_simple op pp_v v
  | LogicalNot -> Fmt.pf ppf "%a%a" unopt_pp_simple op pp_v v
  | _ -> Fmt.pf ppf "%a %a" unopt_pp_simple op pp_v v

let binopt_pp ~(pp_v : 'a Fmt.t) (ppf : Format.formatter)
  ((op, v1, v2) : binopt * 'a * 'a) : unit =
  Fmt.pf ppf "%a %a %a" pp_v v1 binopt_pp_simple op pp_v v2

let triopt_pp ~(pp_v : 'a Fmt.t) (ppf : Format.formatter)
  ((op, v1, v2, v3) : triopt * 'a * 'a * 'a) : unit =
  match op with
  | Conditional -> Fmt.pf ppf "%a ? %a : %a" pp_v v1 pp_v v2 pp_v v3

let nopt_pp ~(pp_v : 'a Fmt.t) (ppf : Format.formatter)
  ((op, vs) : nopt * 'a list) : unit =
  match op with
  | ListExpr -> Fmt.brackets Fmt.(list ~sep:comma pp_v) ppf vs
  | NAryLogicalAnd ->
    Fmt.list ~sep:(fun fmt () -> Fmt.string fmt " && ") pp_v ppf vs
  | NAryLogicalOr ->
    Fmt.list ~sep:(fun fmt () -> Fmt.string fmt " || ") pp_v ppf vs

let unopt_str_simple (op : unopt) : string = Fmt.str "%a" unopt_pp_simple op
[@@inline]

let binopt_str_simple (op : binopt) : string = Fmt.str "%a" binopt_pp_simple op
[@@inline]

let triopt_str_simple (op : triopt) : string = Fmt.str "%a" triopt_pp_simple op
[@@inline]

let nopt_str_simple (op : nopt) : string = Fmt.str "%a" nopt_pp_simple op
[@@inline]

let unopt_str ~(pp_v : 'a Fmt.t) ((op, v) : unopt * 'a) : string =
  Fmt.str "%a" (unopt_pp ~pp_v) (op, v)
[@@inline]

let binopt_str ~(pp_v : 'a Fmt.t) ((op, v1, v2) : binopt * 'a * 'a) : string =
  Fmt.str "%a" (binopt_pp ~pp_v) (op, v1, v2)
[@@inline]

let triopt_str ~(pp_v : 'a Fmt.t) ((op, v1, v2, v3) : triopt * 'a * 'a * 'a) :
  string =
  Fmt.str "%a" (triopt_pp ~pp_v) (op, v1, v2, v3)
[@@inline]

let nopt_str ~(pp_v : 'a Fmt.t) ((op, vs) : nopt * 'a list) : string =
  Fmt.str "%a" (nopt_pp ~pp_v) (op, vs)
[@@inline]
