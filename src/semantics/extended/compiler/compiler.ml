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

type c_expr = Stmt.t list * Expr.t

type c_stmt = Stmt.t list

let ( ?@ ) (e : Expr.t) : Id.t =
  match e.it with
  | Var x -> { it = x; at = e.at }
  | _ -> Log.fail "expecting var expression"

module Const = struct
  let original_main = "main"

  let esl_globals_loc = 0

  let esl_globals_obj = "___internal_esl_global"
end

let real ?(at : at option) (ss : c_stmt) : c_stmt =
  let at at' = { (Option.value ~default:at' at) with real = true } in
  match ss with [] -> [] | s' :: ss -> (s'.it @> at s'.at) :: ss

module Builder = struct
  let var_id = Base.make_name_generator "__v"

  let etrue (x : 'a t) : Expr.t = Expr.Val Value.True @?> x.at

  let efalse (x : 'a t) : Expr.t = Expr.Val Value.False @?> x.at

  let global (x : 'a t) : Expr.t = Expr.Var Const.esl_globals_obj @?> x.at

  let var (at : at) : Expr.t = Expr.Var (var_id ()) @> at

  let block ?(at : at = none) (ss : Stmt.t list) : Stmt.t = Stmt.Block ss @> at

  let block_opt ?(at : at = none) (ss : Stmt.t list) : Stmt.t option =
    match ss with [] -> None | ss -> Some (Stmt.Block ss @> at)

  let throw_checker (at : at) (res : Expr.t) (sthen : c_stmt) : Stmt.t =
    let iserror = Expr.UnOpt (ListHead, res) @?> res.at in
    let rest = Expr.UnOpt (ListTail, res) @?> res.at in
    let value = Expr.UnOpt (ListHead, rest) @?> res.at in
    let selse = Stmt.Assign (?@res, value) @?> at in
    Stmt.If (iserror, block ~at sthen, block_opt ~at [ selse ]) @?> at

  let call_checker (at : at) (res : Expr.t) (ferr : Id.t option) : Stmt.t =
    let sreturn = Stmt.Return res @?> res.at in
    match ferr with
    | None -> throw_checker at res [ sreturn ]
    | Some ferr' ->
      let res' = res.it @?> ferr'.at in
      let ferr'' = Expr.Val (Value.Str ferr'.it) @?> ferr'.at in
      let rest = Expr.UnOpt (ListTail, res') @?> ferr'.at in
      let err = Expr.UnOpt (ListHead, rest) @?> ferr'.at in
      let args = [ global ferr'; err ] in
      let sferr = Stmt.AssignCall (?@res', ferr'', args) @?> ferr'.at in
      let sferr_checker = throw_checker at res' [ sreturn ] in
      throw_checker at res (real [ sferr; sferr_checker ])
end

module SwitchOptimizer = struct
  type case = EExpr.t * EStmt.t

  let is_optimizable (css : case list) : bool =
    match fst (List.split css) with
    | { it = EExpr.Val _; _ } :: { it = EExpr.Val _; _ } :: _ -> true
    | _ -> false

  let compile (at : at) (compile_stmt_f : EStmt.t -> c_stmt)
    (compile_rest_f : Expr.t -> case list -> Stmt.t option) (e_e : Expr.t)
    (css : case list) : c_stmt =
    let rec hash_cases hashed_css = function
      | ({ it = EExpr.Val v; _ }, _) :: css' when Hashtbl.mem hashed_css v ->
        hash_cases hashed_css css'
      | ({ it = EExpr.Val v; _ }, s) :: css' ->
        Hashtbl.replace hashed_css v (Builder.block ~at:s.at (compile_stmt_f s));
        hash_cases hashed_css css'
      | css' -> css'
    in
    let hashed_css = Hashtbl.create !Base.default_hashtbl_sz in
    let css' = hash_cases hashed_css css in
    [ Stmt.Switch (e_e, hashed_css, compile_rest_f e_e css') @?> at ]
end

module MatchWithOptimizer = struct
  type case = EPat.t * EStmt.t

  let pval_opt (pat : EPat.t) (id : Id.t) : Value.t option =
    let get_pv' = function
      | { it = EPat.PatVal.Val v; _ } -> Some v
      | _ -> None
    in
    Option.bind (EPat.patval_opt pat id) get_pv'

  let pval_remove (pat : EPat.t) (id : Id.t) : EPat.t * Value.t =
    let v = Option.get (pval_opt pat id) in
    let pat' = EPat.patval_remove pat id in
    (pat', v)

  let is_pat_optimizable (dsc : Id.t) (pat : EPat.t) : bool =
    Option.is_some (pval_opt pat dsc)

  let is_optimizable (dsc : Id.t) (css : case list) : bool =
    let optimizable' = is_pat_optimizable dsc in
    match css with
    | (pat1, _) :: (pat2, _) :: _ -> optimizable' pat1 && optimizable' pat2
    | _ -> false

  let compile (at : at)
    (compile_case_f : Expr.t -> case -> (unit -> Stmt.t option) -> c_stmt)
    (compile_rest_f : Expr.t -> Id.t -> case list -> Stmt.t option)
    (e_e : Expr.t) (dsc : Id.t) (css : case list) : c_stmt =
    let rec case_replace scase = function
      | { it = Stmt.Block ss; at } ->
        { it = Stmt.Block (case_replace_ss scase ss); at }
      | _ -> Log.fail "expecting if statement"
    and case_replace_ss scase = function
      | { it = Stmt.If (e, s, None); at } :: [] ->
        [ { it = Stmt.If (e, s, Some scase); at } ]
      | { it = Stmt.If (e, s, Some sif); _ } :: [] ->
        [ { it = Stmt.If (e, s, Some (case_replace scase sif)); at } ]
      | s :: ss' -> s :: case_replace_ss scase ss'
      | _ -> Log.fail "expecting if statement"
    in
    let set_case hashed_css v scase =
      match Hashtbl.find_opt hashed_css v with
      | None -> Hashtbl.replace hashed_css v scase
      | Some s -> Hashtbl.replace hashed_css v (case_replace scase s)
    in
    let rec hash_cases hashed_css = function
      | (pat, s) :: css' when is_pat_optimizable dsc pat ->
        let (pat', v) = pval_remove pat dsc in
        let compiled_case = compile_case_f e_e (pat', s) (fun () -> None) in
        let scase = Builder.block ~at:pat'.at compiled_case in
        set_case hashed_css v scase;
        hash_cases hashed_css css'
      | css' -> css'
    in
    let hashed_css = Hashtbl.create !Base.default_hashtbl_sz in
    let dsc_fld = Expr.Val (Value.Str dsc.it) @?> dsc.at in
    let dsc_lookup = Builder.var at in
    let css' = hash_cases hashed_css css in
    [ Stmt.FieldLookup (?@dsc_lookup, e_e, dsc_fld) @?> at
    ; Stmt.Switch (dsc_lookup, hashed_css, compile_rest_f e_e dsc css') @?> at
    ]
end

let compile_sc_and (res : Expr.t) (e1_s : Stmt.t list) (e1_e : Expr.t)
  (e2_s : Stmt.t list) (e2_e : Expr.t) : c_expr =
  let open Builder in
  let rtrue e = block [ Stmt.Assign (?@res, etrue e) @?> e.at ] in
  let rfalse e = block [ Stmt.Assign (?@res, efalse e) @?> e.at ] in
  let e1_test = Expr.BinOpt (Operator.Eq, e1_e, efalse e1_e) @?> e1_e.at in
  let e2_test = Expr.BinOpt (Operator.Eq, e2_e, efalse e2_e) @?> e2_e.at in
  let e2_if = Stmt.If (e2_test, rfalse e2_e, Some (rtrue e2_e)) @?> e2_e.at in
  let e1_else = Builder.block_opt (e2_s @ [ e2_if ]) in
  let e1_if = Stmt.If (e1_test, rfalse e1_e, e1_else) @?> e1_e.at in
  (e1_s @ [ e1_if ], res)

let compile_sc_or (res : Expr.t) (e1_s : Stmt.t list) (e1_e : Expr.t)
  (e2_s : Stmt.t list) (e2_e : Expr.t) : c_expr =
  let open Builder in
  let rtrue e = block [ Stmt.Assign (?@res, etrue e) @?> e.at ] in
  let rfalse e = block [ Stmt.Assign (?@res, efalse e) @?> e.at ] in
  let e1_test = Expr.BinOpt (Operator.Eq, e1_e, etrue e1_e) @?> e1_e.at in
  let e2_test = Expr.BinOpt (Operator.Eq, e2_e, etrue e2_e) @?> e2_e.at in
  let e2_if = Stmt.If (e2_test, rtrue e2_e, Some (rfalse e2_e)) @?> e2_e.at in
  let e1_else = Builder.block_opt (e2_s @ [ e2_if ]) in
  let e1_if = Stmt.If (e1_test, rtrue e1_e, e1_else) @?> e1_e.at in
  (e1_s @ [ e1_if ], res)

let rec compile_expr (s_at : at) (e : EExpr.t) : c_expr =
  match e.it with
  | Val x -> ([], Expr.Val x @> e.at)
  | Var x -> ([], Expr.Var x @> e.at)
  | GVar x -> compile_gvar s_at e x
  | UnOpt (op, e') -> compile_unopt s_at e.at op e'
  | BinOpt (op, e1, e2) -> compile_binopt s_at e.at op e1 e2
  | TriOpt (op, e1, e2, e3) -> compile_triopt s_at e.at op e1 e2 e3
  | NOpt (op, es) -> compile_nopt s_at e.at op es
  | Call (fe, es, ferr) -> compile_call s_at e.at fe es ferr
  | ECall (fn, es) -> compile_ecall s_at e.at fn es
  | NewObj flds -> compile_newobj s_at e.at flds
  | Lookup (oe, fe) -> compile_lookup s_at e.at oe fe
  | Curry (fe, es) -> compile_curry s_at e.at fe es

and compile_gvar (s_at : at) (e : EExpr.t) (x : Id.t') : c_expr =
  let res = Builder.var e.at in
  let global = Builder.global e in
  let entry = Expr.Val (Value.Str x) @?> e.at in
  let sres = Stmt.FieldLookup (?@res, global, entry) @?> s_at in
  ([ sres ], res)

and compile_unopt (s_at : at) (e_at : at) (op : Operator.unopt) (e : EExpr.t) :
  c_expr =
  let (e_s, e_e) = compile_expr s_at e in
  let res = Builder.var e_at in
  let dflt sres = (e_s @ [ sres ], res) in
  match op with
  | ObjectToList -> dflt (Stmt.AssignObjToList (?@res, e_e) @?> s_at)
  | ObjectFields -> dflt (Stmt.AssignObjFields (?@res, e_e) @?> s_at)
  | _ -> begin
    match e_s with
    | [] -> ([], Expr.UnOpt (op, e_e) @?> e_at)
    | _ -> dflt (Stmt.Assign (?@res, Expr.UnOpt (op, e_e) @?> e_at) @?> s_at)
  end

and compile_binopt (s_at : at) (e_at : at) (op : Operator.binopt) (e1 : EExpr.t)
  (e2 : EExpr.t) : c_expr =
  let (e1_s, e1_e) = compile_expr s_at e1 in
  let (e2_s, e2_e) = compile_expr s_at e2 in
  let res = Builder.var e_at in
  let dflt sres = (e1_s @ e2_s @ [ sres ], res) in
  match op with
  | SCLogicalAnd -> compile_sc_and res e1_s e1_e e2_s e2_e
  | SCLogicalOr -> compile_sc_or res e1_s e1_e e2_s e2_e
  | ObjectMem -> dflt (Stmt.AssignInObjCheck (?@res, e1_e, e2_e) @?> e_at)
  | _ -> begin
    match (e1_s, e2_s) with
    | ([], []) -> ([], Expr.BinOpt (op, e1_e, e2_e) @?> e_at)
    | _ ->
      dflt (Stmt.Assign (?@res, Expr.BinOpt (op, e1_e, e2_e) @?> e_at) @?> s_at)
  end

and compile_triopt (s_at : at) (e_at : at) (op : Operator.triopt) (e1 : EExpr.t)
  (e2 : EExpr.t) (e3 : EExpr.t) : c_expr =
  let (e1_s, e1_e) = compile_expr s_at e1 in
  let (e2_s, e2_e) = compile_expr s_at e2 in
  let (e3_s, e3_e) = compile_expr s_at e3 in
  match (e1_s, e2_s, e3_s) with
  | ([], [], []) -> ([], Expr.TriOpt (op, e1_e, e2_e, e3_e) @?> e_at)
  | _ ->
    let res = Builder.var e_at in
    let triopt = Expr.TriOpt (op, e1_e, e2_e, e3_e) @?> e_at in
    let sres = Stmt.Assign (?@res, triopt) @?> s_at in
    (e1_s @ e2_s @ e3_s @ [ sres ], res)

and compile_nopt (s_at : at) (e_at : at) (op : Operator.nopt) (es : EExpr.t list)
  : c_expr =
  let (es_s, es_e) = List.split (List.map (compile_expr s_at) es) in
  let es_s = List.concat es_s in
  match es_s with
  | [] -> ([], Expr.NOpt (op, es_e) @?> e_at)
  | _ ->
    let res = Builder.var e_at in
    let nopt = Expr.NOpt (op, es_e) @?> e_at in
    let sres = Stmt.Assign (?@res, nopt) @?> s_at in
    (es_s @ [ sres ], res)

and compile_call (s_at : at) (e_at : at) (fe : EExpr.t) (es : EExpr.t list)
  (ferr : Id.t option) : c_expr =
  let (fe_s, fe_e) = compile_expr s_at fe in
  let (es_s, es_e) = List.split (List.map (compile_expr s_at) es) in
  let global = Builder.global fe in
  let res = Builder.var e_at in
  let sres = Stmt.AssignCall (?@res, fe_e, global :: es_e) @?> s_at in
  let sthrow = Builder.call_checker s_at (res.it @?> res.at) ferr in
  (fe_s @ List.concat es_s @ (sres :: [ sthrow ]), res)

and compile_ecall (s_at : at) (e_at : at) (fn : Id.t) (es : EExpr.t list) :
  c_expr =
  let (es_s, es_e) = List.split (List.map (compile_expr s_at) es) in
  let res = Builder.var e_at in
  let sres = Stmt.AssignECall (?@res, fn, es_e) @?> s_at in
  (List.concat es_s @ [ sres ], res)

and compile_newobj (s_at : at) (e_at : at) (flds : (Id.t * EExpr.t) list) :
  c_expr =
  let build_fld res (fn, fe) =
    let (fe_s, fe_e) = compile_expr s_at fe in
    let oe = res.it @?> fn.at in
    let fe = Expr.Val (Value.Str fn.it) @?> fn.at in
    let sfassign = Stmt.FieldAssign (oe, fe, fe_e) @?> s_at in
    fe_s @ [ sfassign ]
  in
  let res = Builder.var e_at in
  let snewobj = Stmt.AssignNewObj ?@res @?> s_at in
  let sflds = List.concat (List.map (build_fld res) flds) in
  (snewobj :: sflds, res)

and compile_lookup (s_at : at) (e_at : at) (oe : EExpr.t) (fe : EExpr.t) :
  c_expr =
  let (oe_s, oe_e) = compile_expr s_at oe in
  let (fe_s, fe_e) = compile_expr s_at fe in
  let res = Builder.var e_at in
  let sres = Stmt.FieldLookup (?@res, oe_e, fe_e) @?> s_at in
  (oe_s @ fe_s @ [ sres ], res)

and compile_curry (s_at : at) (e_at : at) (fe : EExpr.t) (es : EExpr.t list) :
  c_expr =
  let (fe_s, fe_e) = compile_expr s_at fe in
  let (es_s, es_e) = List.split (List.map (compile_expr s_at) es) in
  let es_s = List.concat es_s in
  match (fe_s, es_s) with
  | ([], []) -> ([], Expr.Curry (fe_e, es_e) @?> e_at)
  | _ ->
    let res = Builder.var e_at in
    let sres = Stmt.Assign (?@res, Expr.Curry (fe_e, es_e) @?> e_at) @?> s_at in
    (fe_s @ es_s @ [ sres ], res)

let rec compile_stmt (s : EStmt.t) : c_stmt =
  let ( !! ) = real in
  match s.it with
  | Skip -> []
  | Debug s' -> !!(compile_debug s.at s')
  | Block ss -> List.concat_map compile_stmt ss
  | ExprStmt e -> !!(fst (compile_expr s.at e))
  | Print e -> !!(compile_print s.at e)
  | Return e -> !!(compile_return s.at e)
  | Assign (x, _, e) -> !!(compile_assign s.at x e)
  | GAssign (x, e) -> !!(compile_gassign s.at x e)
  | FieldAssign (oe, fe, e) -> !!(compile_fieldassign s.at oe fe e)
  | FieldDelete (oe, fe) -> !!(compile_fielddelete s.at oe fe)
  | If (e, s1, s2) -> !!(compile_if s.at e s1 s2)
  | While (e, s') -> !!(compile_while s.at e s')
  | ForEach (x, e, s') -> !!(compile_foreach s.at x e s')
  | RepeatUntil (s', until) -> !!(compile_repeatuntil s.at s' until)
  | Switch (e, css, dflt) -> !!(compile_switch s.at e css dflt)
  | MatchWith (e, dsc, css) -> !!(compile_matchwith s.at e dsc css)
  | Lambda (x, lid, _, ctxvars, _) -> !!(compile_lambdacall s.at x lid ctxvars)
  | MacroApply (_, _) -> Log.fail "unexpected macro apply stmt"
  | Throw e -> !!(compile_throw s.at e)
  | Fail e -> !!(compile_fail s.at e)
  | Assert e -> !!(compile_assert s.at e)

and compile_debug (at : at) (s : EStmt.t) : c_stmt =
  match compile_stmt s with
  | [] -> Log.fail "expecting non-empty stmt list"
  | s1_s :: ss_s -> (Stmt.Debug s1_s @?> at) :: ss_s

and compile_print (at : at) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr at e in
  e_s @ [ Stmt.Print e_e @?> at ]

and compile_return (at : at) (e : EExpr.t) : c_stmt =
  let e' = if EExpr.isvoid e then EExpr.Val Value.Nothing @?> at else e in
  let (e_s, e_e) = compile_expr at e' in
  let err = Builder.efalse e' in
  let ret = Expr.NOpt (ListExpr, [ err; e_e ]) @?> e'.at in
  e_s @ [ Stmt.Return ret @?> at ]

and compile_assign (at : at) (x : Id.t) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr at e in
  e_s @ [ Stmt.Assign (x, e_e) @?> at ]

and compile_gassign (at : at) (x : Id.t) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr at e in
  let global = Builder.global e in
  let entry = Expr.Val (Value.Str x.it) @?> x.at in
  e_s @ [ Stmt.FieldAssign (global, entry, e_e) @?> at ]

and compile_fieldassign (at : at) (oe : EExpr.t) (fe : EExpr.t) (e : EExpr.t) :
  c_stmt =
  let (oe_s, oe_e) = compile_expr at oe in
  let (fe_s, fe_e) = compile_expr at fe in
  let (e_s, e_e) = compile_expr at e in
  oe_s @ fe_s @ e_s @ [ Stmt.FieldAssign (oe_e, fe_e, e_e) @?> at ]

and compile_fielddelete (at : at) (oe : EExpr.t) (fe : EExpr.t) : c_stmt =
  let (oe_s, oe_e) = compile_expr at oe in
  let (fe_s, fe_e) = compile_expr at fe in
  oe_s @ fe_s @ [ Stmt.FieldDelete (oe_e, fe_e) @?> at ]

and compile_if (at : at) (e : EExpr.t) (s1 : EStmt.t) (s2 : EStmt.t option) :
  c_stmt =
  let (e_s, e_e) = compile_expr at e in
  let sblock = Builder.block ~at:s1.at (compile_stmt s1) in
  let selse = Option.fold ~none:[] ~some:(fun s -> compile_stmt s) s2 in
  e_s @ [ Stmt.If (e_e, sblock, Builder.block_opt selse) @?> at ]

and compile_while (at : at) (e : EExpr.t) (s : EStmt.t) : c_stmt =
  let (e_s, e_e) = compile_expr at e in
  let sblock = Builder.block ~at:s.at (compile_stmt s @ real e_s) in
  e_s @ [ Stmt.While (e_e, sblock) @?> at ]

and compile_foreach (at : at) (x : Id.t) (e : EExpr.t) (s : EStmt.t) : c_stmt =
  let rest = Builder.var { e.at with real = false } in
  let guard_e = Expr.(BinOpt (Ne, rest, Val (List []) @> none)) @> at in
  let (e_s, e_e) = compile_expr at e in
  let s_s = compile_stmt s in
  let e_e' = e_e.it @?> e_e.at in
  let sinit = Stmt.Assign (?@rest, e_e') @?> at in
  let snth = Stmt.Assign (x, Expr.UnOpt (ListHead, rest) @?> at) @?> at in
  let sinc = Stmt.Assign (?@rest, Expr.UnOpt (ListTail, rest) @?> at) @?> at in
  let sblock = Builder.block ~at:s.at ((snth :: s_s) @ real [ sinc ]) in
  e_s @ [ sinit ] @ [ Stmt.While (guard_e, sblock) @?> at ]

and compile_repeatuntil (at : at) (s : EStmt.t) (until : EExpr.t option) :
  c_stmt =
  let e = Option.value ~default:(EExpr.Val Value.False @?> at) until in
  let s_s = compile_stmt s in
  let (e_s, e_e) = compile_expr at e in
  let guard = Expr.UnOpt (LogicalNot, e_e) @?> e.at in
  let block = s_s @ real e_s in
  block @ [ Stmt.While (guard, Builder.block ~at:s.at block) @?> at ]

and compile_switch (at : at) (e : EExpr.t) (css : (EExpr.t * EStmt.t) list)
  (dflt : EStmt.t option) : c_stmt =
  let rec compile_switch' e_e = function
    | [] -> Option.fold dflt ~none:[] ~some:compile_stmt
    | css when SwitchOptimizer.is_optimizable css ->
      SwitchOptimizer.compile at compile_stmt compile_rest e_e css
    | (ei, si) :: css' ->
      let (ei_s, ei_e) = compile_expr at ei in
      let guard = Expr.BinOpt (Eq, e_e, ei_e) @?> ei.at in
      let sblock = Builder.block ~at:si.at (compile_stmt si) in
      let selse = compile_rest e_e css' in
      ei_s @ [ Stmt.If (guard, sblock, selse) @?> ei.at ]
  and compile_rest e_e css = Builder.block_opt (compile_switch' e_e css) in
  let (e_s, e_e) = compile_expr at e in
  e_s @ compile_switch' e_e css

and compile_patv (e_e : Expr.t) (pn : Expr.t) (pv : EPat.PatVal.t)
  (inobj : Expr.t) : c_stmt * Expr.t list * c_stmt =
  match pv.it with
  | Var x -> ([], [], [ Stmt.FieldLookup (x @?> pv.at, e_e, pn) @?> pn.at ])
  | Val v ->
    let fval = Builder.var pn.at in
    let feq = Builder.var pv.at in
    let fval_s = Stmt.FieldLookup (?@fval, e_e, pn) @?> pn.at in
    let feq' = Expr.BinOpt (Eq, fval, Expr.Val v @?> pv.at) @?> pn.at in
    let feq_s = Stmt.Assign (?@feq, feq') @?> pn.at in
    ([ fval_s; feq_s ], [ feq ], [])
  | None ->
    let fnone = Expr.UnOpt (LogicalNot, inobj) @?> pn.at in
    let fnone_s = Stmt.Assign (?@inobj, fnone) @?> pn.at in
    ([ fnone_s ], [], [])

and compile_pat (e_e : Expr.t) (pat : EPat.t) : c_stmt * Expr.t * c_stmt =
  let guard guards = Expr.NOpt (NAryLogicalAnd, guards) @> pat.at in
  let compile_pbs (pre_s, guards, pat_s) (pn, pv) =
    let pn' = Expr.Val (Value.Str pn.it) @?> pn.at in
    let pv' = pv.it @?> pv.at in
    let e_e' = e_e.it @?> e_e.at in
    let inobj = Builder.var pn'.at in
    let sinobj = Stmt.AssignInObjCheck (?@inobj, pn', e_e') @?> pat.at in
    let (pre_s', guards', pat_s') = compile_patv e_e' pn' pv' inobj in
    (pre_s @ (sinobj :: pre_s'), guards @ (inobj :: guards'), pat_s @ pat_s')
  in
  match pat.it with
  | DefaultPat -> ([], guard [], [])
  | ObjPat pbs ->
    let (pre_s, guards, pat_s) = List.fold_left compile_pbs ([], [], []) pbs in
    (pre_s, guard guards, pat_s)

and compile_matchwith (at : at) (e : EExpr.t) (dsc : Id.t option)
  (css : (EPat.t * EStmt.t) list) : c_stmt =
  let is_empty_guard = function
    | { it = Expr.NOpt (NAryLogicalAnd, []); _ } -> true
    | _ -> false
  in
  let compile_case e_e (pat, s) compile_rest_f =
    let pat_at = { pat.at with real = false } in
    let (pre_s, guard_e, pat_s) = compile_pat e_e pat in
    let s_s = compile_stmt s in
    if is_empty_guard guard_e then real (pre_s @ pat_s @ s_s)
    else
      let sblock = Builder.block ~at:pat_at (pat_s @ s_s) in
      real (pre_s @ [ Stmt.If (guard_e, sblock, compile_rest_f ()) @?> pat.at ])
  in
  let open Builder in
  let rec compile_matchwith' e_e dsc = function
    | [] -> []
    | css when MatchWithOptimizer.is_optimizable dsc css ->
      MatchWithOptimizer.compile at compile_case compile_rest e_e dsc css
    | cs :: css' -> compile_case e_e cs (fun () -> compile_rest e_e dsc css')
  and compile_rest e_e dsc css = block_opt (compile_matchwith' e_e dsc css) in
  let (e_s, e_e) = compile_expr at e in
  let src = Builder.var e_e.at in
  let src_s = Stmt.Assign (?@src, e_e) @?> at in
  let dsc' = Option.fold ~none:(Id.default ()) ~some:(fun id -> id) dsc in
  e_s @ [ src_s ] @ compile_matchwith' e_e dsc' css

and compile_lambdacall (at : at) (x : Id.t) (id : string) (ctxvars : Id.t list)
  : c_stmt =
  let ctxvars' = List.map (fun x -> Expr.Var x.it @> x.at) ctxvars in
  let curry = Expr.Curry (Expr.Val (Value.Str id) @?> at, ctxvars') @> at in
  [ Stmt.Assign (x, curry) @?> at ]

and compile_throw (at : at) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr at e in
  let err = Builder.etrue e in
  let ret = Expr.NOpt (ListExpr, [ err; e_e ]) @?> e.at in
  e_s @ [ Stmt.Return ret @?> at ]

and compile_fail (at : at) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr at e in
  e_s @ [ Stmt.Fail e_e @?> at ]

and compile_assert (at : at) (e : EExpr.t) : c_stmt =
  let (e_s, e_e) = compile_expr at e in
  e_s @ [ Stmt.Assert e_e @?> at ]

let compile_func (f : EFunc.t) : Func.t =
  let (fn, pxs, s) = EFunc.(name f, params f, body f) in
  let global = Const.esl_globals_obj @?> fn.at in
  let s_s = compile_stmt s in
  if fn.it = Const.original_main then
    let s_s' = (Stmt.AssignNewObj global @?> fn.at) :: s_s in
    Func.create fn pxs (Builder.block ~at:s.at s_s') @> f.at
  else
    let params' = global :: pxs in
    Func.create fn params' (Builder.block ~at:s.at s_s) @> f.at

let compile_lambda
  ((at, id, pxs, ctxvars, s) : at * string * Id.t list * Id.t list * EStmt.t) :
  Func.t =
  let global = Const.esl_globals_obj @?> at in
  let s_s = compile_stmt s in
  let params = ctxvars @ (global :: pxs) in
  Func.create (id @?> at) params (Builder.block ~at:s.at s_s) @> at

let compile_prog (p : EProg.t) : Prog.t =
  let funcs_f _ f acc = compile_func f :: acc in
  let funcs = Hashtbl.fold funcs_f (EProg.funcs p) [] in
  let lambdas = List.map compile_lambda (EProg.lambdas p) in
  Prog.create (lambdas @ funcs)
