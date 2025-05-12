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

open Ecma_sl

(* TODO: Usage of Fmt.failwith with be reduced in the future with Result.t *)

type value = Smtml.Expr.t

type t = value

let null = Smtml.Expr.value Nothing

let string s = Smtml.Expr.value (Str s)

let equal a b = Smtml.Expr.equal a b [@@inline]

let hash e = Smtml.Expr.hash e [@@inline]

let compare a b = Smtml.Expr.compare a b

let pp fmt v = Smtml.Expr.pp fmt v [@@inline]

let to_string v = Smtml.Expr.to_string v

let mk_symbol x = Smtml.Expr.value (App (`Op "symbol", [ Str x ])) [@@inline]

let mk_list vs = Smtml.Expr.list vs [@@inline]

let mk_tuple (fst, snd) = Smtml.Expr.list [ fst; snd ] [@@inline]

let is_symbolic v = Smtml.Expr.is_symbolic v

let func v =
  match Smtml.Expr.view v with
  | Val (Value.Str func) -> Ok (func, [])
  | App (symbol, args) ->
    let f = match symbol.name with Simple f -> f | _ -> assert false in
    Ok (f, args)
  | _ ->
    Error (Fmt.str "Value: '%a' is not a function identifier" Smtml.Expr.pp v)

module Bool = struct
  include Smtml.Expr.Bool

  let const b = if b then true_ else false_

  let not_ e = not e [@@inline]
end

module Store = struct
  module SMap = Map.Make (String)

  type bind = string

  type t = value SMap.t

  let create (values : (bind * value) list) : t =
    List.fold_left
      (fun acc (key, data) -> SMap.add key data acc)
      SMap.empty values

  let mem (store : t) (x : bind) : bool = SMap.mem x store

  let add_exn (store : t) (key : bind) (data : value) : t =
    SMap.add key data store

  let find (store : t) (x : bind) : value option = SMap.find_opt x store

  let pp ppf store =
    let open Fmt in
    let iter f m =
      SMap.iter
        (fun k v -> f (k, v) )
          (* if not @@ String.starts_with ~prefix:"__" k then f (k, v)) *)
        m
    in
    let pp_v ppf (k, v) = Fmt.pf ppf "@[<hov 1>%s@ ->@ %a@]" k pp v in
    Fmt.pf ppf "@[<hov 1>{ %a }@]" (Fmt.iter ~sep:semi iter pp_v) store
end

type store = Store.t

(* FIXME: this function should be in smtml *)
let rec expr_type v1 =
  match Smtml.Expr.view v1 with
  | Smtml.Expr.Unop (Ty_str, Length, _) -> Smtml.Ty.Ty_int
  | Relop (_, _, _, _) -> Ty_bool
  | Triop (_, Ite, _, a, _) -> expr_type a
  | _ -> Smtml.Expr.ty v1

let eval_unop (op : Operator.unopt) =
  match op with
  | Neg -> (
    fun v ->
      let t = expr_type v in
      match t with
      | Ty_int -> Smtml.Expr.unop Ty_int Neg v
      | Ty_real -> Smtml.Expr.unop Ty_real Neg v
      | _ ->
        Fmt.failwith "eval_unop: unsupported (neg (%a : %a))" Smtml.Expr.pp v
          Smtml.Ty.pp t )
  | BitwiseNot -> Smtml.Expr.unop Ty_int Not
  | LogicalNot -> Smtml.Expr.unop Ty_bool Not
  | ListHead -> Smtml.Expr.unop Ty_list Head
  | ListTail -> Smtml.Expr.unop Ty_list Tail
  | Typeof -> (
    fun v ->
      match Smtml.Expr.view v with
      | Val v' -> Smtml.Expr.value (Eval_op.typeof_semantics (v', Source.none))
      | Relop _ -> Smtml.Expr.value (Str "bool")
      | Cvtop (Ty_real, ToString, _) -> Smtml.Expr.value (Str "string")
      | Cvtop (Ty_str, String_to_float, _) -> Smtml.Expr.value (Str "float")
      | _ -> (
        match Smtml.Expr.ty v with
        | Ty_int -> Smtml.Expr.value (Str "int")
        | Ty_real -> Smtml.Expr.value (Str "float")
        | Ty_bool -> Smtml.Expr.value (Str "bool")
        | Ty_str -> Smtml.Expr.value (Str "string")
        | Ty_list -> Smtml.Expr.value (Str "list")
        | Ty_app -> Smtml.Expr.value (Str "app")
        | _ ->
          Fmt.failwith "eval_unop: unsupported (typeof (%a : %a))" Smtml.Expr.pp
            v Smtml.Expr.pp v ) )
  | IntToFloat -> Smtml.Expr.cvtop Ty_real Reinterpret_int
  | IntToString -> Smtml.Expr.cvtop Ty_int ToString
  | FloatToInt -> Smtml.Expr.cvtop Ty_int Reinterpret_float
  | FloatToString -> (
    fun v ->
      match Smtml.Expr.view v with
      | Val (Real f) ->
        Smtml.Expr.value (Str (Arith_utils.float_to_string_inner f))
      | _ -> Smtml.Expr.cvtop Ty_real ToString v )
  | StringToInt -> Smtml.Expr.cvtop Ty_int OfString
  | StringToFloat -> (
    fun v ->
      try Smtml.Expr.cvtop Ty_real OfString v
      with _ -> Smtml.Expr.value (Real Float.nan) )
  | ObjectToList -> assert false
  | ObjectFields -> assert false

let eval_binop (op : Operator.binopt) =
  match op with
  | Plus -> (
    fun v1 v2 ->
      let (t1, t2) = (expr_type v1, expr_type v2) in
      match (t1, t2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Add v1 v2
      | (Ty_real, Ty_real) -> Smtml.Expr.binop Ty_real Add v1 v2
      | (Ty_str, Ty_str) -> Smtml.Expr.naryop Ty_str Concat [ v1; v2 ]
      | (Ty_str, _) | (_, Ty_str) -> Smtml.Expr.naryop Ty_str Concat [ v1; v2 ]
      | _ ->
        Fmt.failwith "eval_binop: unsupported (plus (%a : %a) (%a : %a)"
          Smtml.Expr.pp v1 Smtml.Ty.pp t1 Smtml.Expr.pp v2 Smtml.Ty.pp t2 )
  | Minus -> (
    fun v1 v2 ->
      let (t1, t2) = (expr_type v1, expr_type v2) in
      match (t1, t2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Sub v1 v2
      | (Ty_real, Ty_real) -> Smtml.Expr.binop Ty_real Sub v1 v2
      | _ ->
        Fmt.failwith "eval_binop: unsupported (minus (%a : %a) (%a : %a)"
          Smtml.Expr.pp v1 Smtml.Ty.pp t1 Smtml.Expr.pp v2 Smtml.Ty.pp t2 )
  | Times -> (
    fun v1 v2 ->
      let (t1, t2) = (expr_type v1, expr_type v2) in
      match (t1, t2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Mul v1 v2
      | (Ty_real, Ty_real) -> Smtml.Expr.binop Ty_real Mul v1 v2
      | _ ->
        Fmt.failwith "eval_binop: unsupported (times (%a : %a) (%a : %a)"
          Smtml.Expr.pp v1 Smtml.Ty.pp t1 Smtml.Expr.pp v2 Smtml.Ty.pp t2 )
  | Div -> (
    fun v1 v2 ->
      let (t1, t2) = (expr_type v1, expr_type v2) in
      match (t1, t2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Div v1 v2
      | (Ty_real, Ty_real) -> Smtml.Expr.binop Ty_real Div v1 v2
      | _ ->
        Fmt.failwith "eval_binop: unsupported (div (%a : %a) (%a : %a)"
          Smtml.Expr.pp v1 Smtml.Ty.pp t1 Smtml.Expr.pp v2 Smtml.Ty.pp t2 )
  | Modulo -> (
    fun v1 v2 ->
      match (expr_type v1, expr_type v2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.binop Ty_int Rem v1 v2
      | (Ty_real, Ty_real) ->
        let v1 = Smtml.Expr.cvtop Ty_int Reinterpret_float v1 in
        let v2 = Smtml.Expr.cvtop Ty_int Reinterpret_float v2 in
        Smtml.Expr.cvtop Ty_real Reinterpret_int
          (Smtml.Expr.binop Ty_int Rem v1 v2)
      | _ -> assert false )
  | Pow -> Smtml.Expr.binop Ty_real Pow
  | BitwiseAnd -> Smtml.Expr.binop Ty_int And
  | BitwiseOr -> Smtml.Expr.binop Ty_int Or
  | BitwiseXor -> Smtml.Expr.binop Ty_int Xor
  | ShiftLeft -> Smtml.Expr.binop Ty_int Shl
  | ShiftRight -> Smtml.Expr.binop Ty_int ShrA
  | ShiftRightLogical -> Smtml.Expr.binop Ty_int ShrL
  | LogicalAnd -> Smtml.Expr.binop Ty_bool And
  | LogicalOr -> Smtml.Expr.binop Ty_bool Or
  | SCLogicalAnd -> assert false
  | SCLogicalOr -> assert false
  | Eq -> (
    fun v1 v2 ->
      match (expr_type v1, expr_type v2) with
      | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Eq v1 v2
      | _ -> Smtml.Expr.relop Ty_bool Eq v1 v2 )
  | Ne -> (
    fun v1 v2 ->
      match (expr_type v1, expr_type v2) with
      | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Ne v1 v2
      | _ -> Smtml.Expr.relop Ty_bool Ne v1 v2 )
  | Lt -> (
    fun v1 v2 ->
      let (t1, t2) = (expr_type v1, expr_type v2) in
      match (t1, t2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.relop Ty_int Lt v1 v2
      | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Lt v1 v2
      | (Ty_str, Ty_str) -> Smtml.Expr.relop Ty_str Lt v1 v2
      | _ -> Smtml.Expr.relop Ty_int Lt v1 v2 )
  | Le -> (
    fun v1 v2 ->
      let (t1, t2) = (expr_type v1, expr_type v2) in
      match (t1, t2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.relop Ty_int Le v1 v2
      | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Le v1 v2
      | (Ty_str, Ty_str) -> Smtml.Expr.relop Ty_str Le v1 v2
      | _ ->
        Fmt.failwith "eval_binop: unsupported (le (%a : %a) (%a : %a)"
          Smtml.Expr.pp v1 Smtml.Ty.pp t1 Smtml.Expr.pp v2 Smtml.Ty.pp t2 )
  | Gt -> (
    fun v1 v2 ->
      let (t1, t2) = (expr_type v1, expr_type v2) in
      match (t1, t2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.relop Ty_int Gt v1 v2
      | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Gt v1 v2
      | (Ty_str, Ty_str) -> Smtml.Expr.relop Ty_str Gt v1 v2
      | _ ->
        Fmt.failwith "eval_binop: unsupported (gt (%a : %a) (%a : %a)"
          Smtml.Expr.pp v1 Smtml.Ty.pp t1 Smtml.Expr.pp v2 Smtml.Ty.pp t2 )
  | Ge -> (
    fun v1 v2 ->
      let (t1, t2) = (expr_type v1, expr_type v2) in
      match (t1, t2) with
      | (Ty_int, Ty_int) -> Smtml.Expr.relop Ty_int Ge v1 v2
      | (Ty_real, Ty_real) -> Smtml.Expr.relop Ty_real Ge v1 v2
      | (Ty_str, Ty_str) -> Smtml.Expr.relop Ty_str Ge v1 v2
      | _ ->
        Fmt.failwith "eval_binop: unsupported (ge (%a : %a) (%a : %a)"
          Smtml.Expr.pp v1 Smtml.Ty.pp t1 Smtml.Expr.pp v2 Smtml.Ty.pp t2 )
  | ObjectMem -> assert false

let eval_triop (op : Operator.triopt) =
  match op with Conditional -> Smtml.Expr.(triop Ty_bool Ite)

let eval_nop (op : Operator.nopt) =
  match op with
  | NAryLogicalAnd -> Smtml.Expr.naryop Ty_bool Logand
  | NAryLogicalOr -> Smtml.Expr.naryop Ty_bool Logor
  | ListExpr ->
    (* TODO:x to check if this is right *)
    fun vs -> Smtml.Expr.list vs

let rec eval_expr (store : store) (e : Expr.t) : value =
  match e.it with
  | Val (List lst) -> Smtml.Expr.list (List.map Smtml.Expr.value lst)
  | Val v -> Smtml.Expr.value v
  | Var x -> (
    match Store.find store x with
    | Some v -> v
    | None -> Fmt.failwith "Cannot find var '%s'" x )
  | UnOpt (op, e) ->
    let e' = eval_expr store e in
    eval_unop op e'
  | BinOpt (op, e1, e2) ->
    let e1' = eval_expr store e1 in
    let e2' = eval_expr store e2 in
    eval_binop op e1' e2'
  | TriOpt (op, e1, e2, e3) ->
    let e1' = eval_expr store e1 in
    let e2' = eval_expr store e2 in
    let e3' = eval_expr store e3 in
    eval_triop op e1' e2' e3'
  | NOpt (op, es) ->
    let es' = List.map (eval_expr store) es in
    eval_nop op es'
  | Curry (f, es) -> (
    let f' = eval_expr store f in
    let es' = List.map (eval_expr store) es in
    match Smtml.Expr.view f' with
    | Val (Value.Str f') -> Smtml.Expr.app Smtml.Symbol.(mk term f') es'
    | _ ->
      Fmt.failwith "eval_expr: unsupported function name: %a" Smtml.Expr.pp f' )
