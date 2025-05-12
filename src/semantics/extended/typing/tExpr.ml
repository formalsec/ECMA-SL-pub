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
open EslSyntax.Source

let rec type_expr (tctx : TCtx.t) (e : EExpr.t) : EType.t =
  type_expr' tctx e @?> e.at

and type_expr' (tctx : TCtx.t) (e : EExpr.t) : EType.t' =
  let texprs es = List.map (type_expr tctx) es in
  let tflds flds = List.map (fun (fn, fe) -> (fn, (type_expr tctx) fe)) flds in
  match e.it with
  | Val v -> type_val v
  | Var x -> type_var tctx (x @?> e.at)
  | GVar _ -> AnyType (* TODO: global variables *)
  | UnOpt (op, e') -> texprs [ e' ] |> TOperator.type_unopt op
  | BinOpt (op, e1, e2) -> texprs [ e1; e2 ] |> TOperator.type_binopt op
  | TriOpt (op, e1, e2, e3) -> texprs [ e1; e2; e3 ] |> TOperator.type_triopt op
  | NOpt (op, es) -> texprs es |> TOperator.type_nopt op
  | Call (_, _, None) -> AnyType (* TODO: function calls *)
  | Call (_, _, Some _) -> AnyType (* TODO: function calls with throws *)
  | ECall _ -> AnyType (* TODO: external calls *)
  | NewObj flds -> tflds flds |> type_object
  | Lookup _ -> AnyType (* TODO: field lookups *)
  | Curry _ -> AnyType (* TODO: curry expressions *)

and type_val (v : Value.t) : EType.t' =
  match v with
  | Nothing -> NullType
  | Unit -> VoidType
  | Int i -> LiteralType (LitWeak, IntegerLit i)
  | Real f -> LiteralType (LitWeak, FloatLit f)
  | Str s -> LiteralType (LitWeak, StringLit s)
  | True -> LiteralType (LitWeak, BooleanLit true)
  | False -> LiteralType (LitWeak, BooleanLit false)
  | App (`Op "symbol", [ Str "undefined" ]) -> UndefinedType
  | App (`Op "symbol", [ Str s ]) -> LiteralType (LitWeak, SymbolLit s)
  | App (`Op "loc", [ Int _ ]) -> Log.fail "loc val"
  (* TODO:x | Arr _ -> Log.fail "array val" *)
  | List _ -> Log.fail "list val"
  (* | Type _ -> AnyType (* TODO *) *)
  (* | Curry _ -> AnyType (* TODO *) *)
  | _ -> AnyType

and type_var (tctx : TCtx.t) (x : Id.t) : EType.t' =
  match TCtx.tenv_find tctx x with
  | None -> Typing_error.(throw ~src:x.at (UnknownVar x.it))
  | Some t -> t.tref

and type_object (flds : (Id.t * EType.t) list) : EType.t' =
  let set_object_field_f tflds (fn, ft) =
    if not (Hashtbl.mem tflds fn.it) then
      Hashtbl.replace tflds fn.it (fn, ft, EType.FldReq)
    else Log.fail "unexpected dup object fld"
  in
  let tflds = Hashtbl.create !Base.default_hashtbl_sz in
  List.iter (set_object_field_f tflds) flds;
  ObjectType { kind = ObjLit; flds = tflds; smry = None }
