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
open Source

type tlitkind =
  | LitWeak
  | LitStrong

let equal_tlitkind a b =
  match (a, b) with
  | (LitWeak, LitWeak) | (LitStrong, LitStrong) -> true
  | ((LitWeak | LitStrong), _) -> false

type tliteral =
  | IntegerLit of int
  | FloatLit of float
  | BooleanLit of bool
  | StringLit of string
  | SymbolLit of string

type tobjkind =
  | ObjLit
  | ObjSto

let equal_toobjkind a b =
  match (a, b) with
  | (ObjLit, ObjLit) | (ObjSto, ObjSto) -> true
  | ((ObjLit | ObjSto), _) -> false

type tfldstyle =
  | FldReq
  | FldOpt

type t = t' Source.t

and t' =
  | AnyType
  | UnknownType
  | NeverType
  | UndefinedType
  | NullType
  | VoidType
  | IntType
  | FloatType
  | StringType
  | BooleanType
  | SymbolType
  | LiteralType of tlitkind * tliteral
  | ObjectType of tobject
  | ListType of t
  | TupleType of t list
  | UnionType of t list
  | SigmaType of Id.t * t list
  | UserDefinedType of Id.t'

and tobject =
  { kind : tobjkind
  ; flds : (Id.t', tobjfld) Hashtbl.t
  ; smry : (Id.t * t) option
  }

and tobjfld = Id.t * t * tfldstyle

let resolve_topt (t : t option) : t =
  match t with Some t' -> t' | None -> AnyType @> none

let tliteral_to_val (lt : tliteral) : Value.t =
  match lt with
  | IntegerLit i -> Value.Int i
  | FloatLit f -> Value.Real f
  | StringLit s -> Value.Str s
  | BooleanLit b -> if b then Value.True else Value.False
  | SymbolLit s -> Value.App (`Op "symbol", [ Value.Str s ])

let tliteral_to_wide (lt : tliteral) : t' =
  match lt with
  | IntegerLit _ -> IntType
  | FloatLit _ -> FloatType
  | StringLit _ -> StringType
  | BooleanLit _ -> BooleanType
  | SymbolLit _ -> SymbolType

let rec equal (t1 : t) (t2 : t) : bool =
  let tfld_equal (fn1, ft1, fs1) (fn2, ft2, fs2) =
    String.equal fn1.it fn2.it && equal ft1 ft2 && phys_equal fs1 fs2
  in
  match (Source.view t1, Source.view t2) with
  | (AnyType, AnyType) -> true
  | (UnknownType, UnknownType) -> true
  | (NeverType, NeverType) -> true
  | (UndefinedType, UndefinedType) -> true
  | (NullType, NullType) -> true
  | (VoidType, VoidType) -> true
  | (IntType, IntType) -> true
  | (FloatType, FloatType) -> true
  | (StringType, StringType) -> true
  | (BooleanType, BooleanType) -> true
  | (SymbolType, SymbolType) -> true
  | (LiteralType (_, lt1), LiteralType (_, lt2)) ->
    Value.equal (tliteral_to_val lt1) (tliteral_to_val lt2)
  | (ObjectType tobj1, ObjectType tobj2) ->
    let tflds1 = Hashtbl.to_seq_values tobj1.flds in
    let tflds2 = Hashtbl.to_seq_values tobj2.flds in
    equal_toobjkind tobj1.kind tobj2.kind
    && Seq.length tflds1 = Seq.length tflds2
    && Seq.for_all (fun tfld1 -> Seq.exists (tfld_equal tfld1) tflds2) tflds1
    && equal_smry tobj1.smry tobj2.smry
  | (ListType t1, ListType t2) -> equal t1 t2
  | (TupleType ts1, TupleType ts2) -> List.equal equal ts1 ts2
  | (UnionType ts1, UnionType ts2) -> List.equal equal ts1 ts2
  | (SigmaType (dsc1, ts1), SigmaType (dsc2, ts2)) ->
    Id.equal dsc1 dsc2 && List.equal equal ts1 ts2
  | (UserDefinedType tvar1, UserDefinedType tvar2) -> String.equal tvar1 tvar2
  | _ -> false

and equal_smry a b =
  match (a, b) with
  | (None, None) -> true
  | (Some (_, a), Some (_, b)) -> equal a b
  | ((Some _ | None), _) -> false

let pp_tobjfld (ppf : Format.formatter) ((fn, ft, fs) : tobjfld) : unit =
  let str_opt = function FldOpt -> "?" | _ -> "" in
  Fmt.pf ppf "%a%s: %a" Id.pp fn (str_opt fs) pp ft

let rec pp (ppf : Format.formatter) (t : t) : unit =
  match t.it with
  | AnyType -> Fmt.string ppf "any"
  | UnknownType -> Fmt.string ppf "unknown"
  | NeverType -> Fmt.string ppf "never"
  | UndefinedType -> Fmt.string ppf "undefined"
  | NullType -> Fmt.string ppf "null"
  | VoidType -> Fmt.string ppf "void"
  | IntType -> Fmt.string ppf "int"
  | FloatType -> Fmt.string ppf "float"
  | StringType -> Fmt.string ppf "string"
  | BooleanType -> Fmt.string ppf "boolean"
  | SymbolType -> Fmt.string ppf "symbol"
  | LiteralType (_, tl) -> Value.pp ppf (tliteral_to_val tl)
  | ObjectType { flds; smry; _ } when Hashtbl.length flds = 0 ->
    let pp_smry ppf (_, tsmry) = Fmt.pf ppf " *: %a " pp tsmry in
    Fmt.pf ppf "{%a}" (Fmt.option pp_smry) smry
  | ObjectType { flds; smry; _ } ->
    let pp_tfld ppf (_, tfld) = pp_tobjfld ppf tfld in
    let pp_smry ppf (_, tsmry) = Fmt.pf ppf ", *: %a" pp tsmry in
    Fmt.(
      pf ppf "@[<h>{ %a%a }@]"
        (hashtbl ~sep:comma pp_tfld)
        flds (option pp_smry) smry )
  | ListType t' -> Fmt.pf ppf "%a[]" pp t'
  | TupleType ts ->
    Fmt.(parens (list ~sep:(fun fmt () -> string fmt " * ") pp)) ppf ts
  | UnionType ts ->
    Fmt.(parens (list ~sep:(fun fmt () -> string fmt " | ") pp)) ppf ts
  | SigmaType (dsc, ts) ->
    Fmt.(
      pf ppf "sigma [%a] | %a" Id.pp dsc
        (list ~sep:(fun fmt () -> string fmt " | ") pp)
        ts )
  | UserDefinedType tvar -> Fmt.string ppf tvar

let str (t : t) : string = Fmt.str "%a" pp t

let tannot_pp (ppf : Format.formatter) (t : t option) =
  Fmt.option (fun ppf t -> Fmt.pf ppf ": %a" pp t) ppf t

module TDef = struct
  type tval = t

  type t =
    { name : Id.t
    ; tval : tval
    }

  let create (name : Id.t) (tval : tval) : t = { name; tval }

  let name (tdef : t) : Id.t = tdef.name

  let name' (tdef : t) : Id.t' = tdef.name.it

  let tval (tdef : t) : tval = tdef.tval

  let pp (ppf : Format.formatter) (tdef : t) : unit =
    let { name = tn; tval = tv } = tdef in
    Fmt.pf ppf "typedef %a := %a;" Id.pp tn pp tv

  let str (tdef : t) : string = Fmt.str "%a" pp tdef
end
