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

let type_func_params (f : EFunc.t) (tctx : TCtx.t) : TCtx.t =
  let tparam pt = TCtx.tvar_create pt (EType.resolve_topt pt) in
  let set_param (px, pt) =
    match TCtx.tenv_find tctx px with
    | Some _ -> Log.fail "unexpected dup param"
    | None -> TCtx.tenv_set tctx px (tparam pt)
  in
  TCtx.tenv_reset tctx;
  let tpxs = EFunc.tparams f in
  List.iter set_param tpxs;
  tctx

let type_func code (_ : Id.t') (f : EFunc.t) (tctx : TCtx.t) : TCtx.t =
  let tctx' = TCtx.set_func f tctx in
  type_func_params f tctx' |> TStmt.type_stmt code (EFunc.body f)

let type_prog code (p : EProg.t) : bool =
  let tctx = TCtx.create p in
  let tctx = Hashtbl.fold (type_func code) (EProg.funcs p) tctx in
  tctx.tsafe
