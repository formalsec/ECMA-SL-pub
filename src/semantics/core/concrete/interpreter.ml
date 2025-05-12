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
include Interpreter_base
open EslBase
open EslSyntax
open EslSyntax.Source

module M (ITooling : Interpreter_tooling.M) = struct
  type st =
    { store : store
    ; heap : heap
    ; stack : stack
    ; itool : ITooling.t ref
    }

  type return =
    | Final of Value.t
    | Error of Value.t
    | Intermediate of st * Stmt.t list

  let set_global_var (store : store) (heap : heap) : unit =
    let open Compiler.Const in
    if Option.is_some (Heap.get heap IConst.global_loc) then
      Store.set store esl_globals_obj (Value.loc IConst.global_loc)

  let initial_state (heap' : heap option) (itool : ITooling.t ref)
    (main : Func.t) : st =
    let store = Store.create [] in
    let heap = Option.fold ~none:(Heap.create ()) ~some:Heap.extend heap' in
    let stack = Call_stack.create main in
    set_global_var store heap;
    { store; heap; stack; itool }

  let get_obj (heap : heap) (l : Loc.t) : obj =
    match Heap.get heap l with
    | None -> Log.fail "expecting existing location, but got %a" Loc.pp l
    | Some obj -> obj

  let get_var (store : store) (x : string) (at : at) : Value.t =
    match Store.get store x with
    | None -> Runtime_error.(throw ~src:at (UnknownVar x))
    | Some v -> v

  let get_func (p : Prog.t) (fn : string) (at : at) : Func.t =
    match Prog.func p fn with
    | None -> Runtime_error.(throw ~src:at (UnknownFunc fn))
    | Some f -> f

  let eval_op_semantics (op_lbl_f : unit -> string) (op_eval_f : unit -> Value.t)
    : Value.t =
    try op_eval_f () with
    | Runtime_error.Error err ->
      Runtime_error.(push (OpEvalExn (op_lbl_f ())) err |> raise)
    | err -> Log.fail "unexpected operator error: %s" (Printexc.to_string err)

  let rec eval_expr (st : st) (e : Expr.t) : Value.t =
    let v = eval_expr' st e in
    let lvl = Call_stack.depth st.stack - 1 in
    ITooling.Profiler.count !(st.itool).pf `Expr;
    ITooling.Tracer.trace_expr !(st.itool).code lvl e (st.heap, v);
    v

  and eval_expr' (st : st) (e : Expr.t) : Value.t =
    match e.it with
    | Val v -> v
    | Var x -> get_var st.store x e.at
    | UnOpt (op, e') ->
      let arg = (eval_expr st e', e'.at) in
      let op_lbl_f () = Operator.unopt_label op in
      let op_eval_f () = Eval_op.unopt_semantics op arg in
      eval_op_semantics op_lbl_f op_eval_f
    | BinOpt (op, e1, e2) ->
      let arg1 = (eval_expr st e1, e1.at) in
      let arg2 = (eval_expr st e2, e2.at) in
      let op_lbl_f () = Operator.binopt_label op in
      let op_eval_f () = Eval_op.binopt_semantics op (arg1, arg2) in
      eval_op_semantics op_lbl_f op_eval_f
    | TriOpt (op, e1, e2, e3) ->
      let arg1 = (eval_expr st e1, e1.at) in
      let arg2 = (eval_expr st e2, e2.at) in
      let arg3 = (eval_expr st e3, e3.at) in
      let op_lbl_f () = Operator.triopt_label op in
      let op_eval_f () = Eval_op.triopt_semantics op (arg1, arg2, arg3) in
      eval_op_semantics op_lbl_f op_eval_f
    | NOpt (op, es) ->
      let args = List.map (fun e -> (eval_expr st e, e.at)) es in
      let op_lbl_f () = Operator.nopt_label op in
      let op_eval_f () = Eval_op.nopt_semantics op args in
      eval_op_semantics op_lbl_f op_eval_f
    | Curry (fe, es) -> (
      let fv = eval_expr st fe in
      let vs = List.map (eval_expr st) es in
      match fv with
      | Str fn -> Value.App (`Op fn, vs)
      | _ -> Runtime_error.(throw ~src:fe.at (BadExpr ("curry", fv))) )

  let eval_str (st : st) (e : Expr.t) : string =
    match eval_expr st e with
    | Str s -> s
    | _ as v -> Runtime_error.(throw ~src:e.at (BadVal ("string", v)))

  let eval_bool (st : st) (e : Expr.t) : bool =
    match eval_expr st e with
    | Value.True -> true
    | Value.False -> false
    | _ as v -> Runtime_error.(throw ~src:e.at (BadVal ("boolean", v)))

  let eval_loc (st : st) (e : Expr.t) : Loc.t =
    match eval_expr st e with
    | App (`Op "loc", [ Int l ]) -> l
    | _ as v -> Runtime_error.(throw ~src:e.at (BadVal ("location", v)))

  let eval_func_expr (st : st) (fe : Expr.t) : string * Value.t list =
    match eval_expr st fe with
    | Value.Str fn -> (fn, [])
    | Value.App (`Op fn, fvs) -> (fn, fvs)
    | _ as v -> Runtime_error.(throw ~src:fe.at (BadFuncId v))

  let eval_obj (st : st) (e : Expr.t) : Loc.t * obj =
    let loc = eval_loc st e in
    let obj = get_obj st.heap loc in
    (loc, obj)

  let rec rec_print_pp (depth : int option) (visited : (Loc.t, unit) Hashtbl.t)
    (heap : heap) (ppf : Format.formatter) (v : Value.t) : unit =
    let inv_depth_f = Option.fold ~none:false ~some:(fun d -> d <= 0) in
    let incr_depth_f = Option.map (fun d -> d - 1) in
    let visited_loc_f = Hashtbl.mem visited in
    let rec_print_pp' = rec_print_pp (incr_depth_f depth) visited heap in
    match v with
    | List _ when inv_depth_f depth -> Fmt.pf ppf "[...]"
    | App (`Op "loc", [ Int l ]) when inv_depth_f depth || visited_loc_f l ->
      Fmt.pf ppf "{...}"
    | App (`Op "loc", [ Int l ]) ->
      Hashtbl.add visited l ();
      (Object.pp rec_print_pp') ppf (get_obj heap l);
      Hashtbl.remove visited l
    | _ -> Value.pp_custom_val rec_print_pp' ppf v

  let print_pp (heap : heap) (ppf : Format.formatter) (v : Value.t) : unit =
    let mk_visited () = Hashtbl.create !Base.default_hashtbl_sz in
    match v with
    | Str s -> Fmt.string ppf s
    | _ -> rec_print_pp !IConfig.print_depth (mk_visited ()) heap ppf v

  let prepare_store_binds (pxs : string list) (vs : Value.t list) (at : at) :
    (string * Value.t) list =
    try List.combine pxs vs
    with Invalid_argument _ ->
      let (npxs, nargs) = (List.length pxs, List.length vs) in
      Runtime_error.(throw ~src:at (BadNArgs (npxs, nargs)))

  let prepare_call (st : st) (f : Func.t) (cont : Stmt.t list) (x : string)
    (vs : Value.t list) (at : at) : stack * store =
    let pxs = Func.params' f in
    let stack' = Call_stack.push st.stack f st.store cont x in
    let store' = Store.create (prepare_store_binds pxs vs at) in
    (stack', store')

  let eval_small_step (p : Prog.t) (st : st) (s : Stmt.t) (cont : Stmt.t list) :
    return =
    let itool = !(st.itool) in
    let lbl_f = ITooling.Monitor.update_label in
    let ( $$ ) v s_eval = lbl_f itool.mon s s_eval |> fun () -> v in
    let lvl = Call_stack.depth st.stack - 1 in
    ITooling.Tracer.trace_stmt !(st.itool).code lvl s;
    ITooling.Profiler.count itool.pf `Stmt;
    match s.it with
    | Skip -> Intermediate (st, cont) $$ SkipEval
    | Merge -> Intermediate (st, cont) $$ MergeEval
    | Debug s' ->
      let db_st = (st.store, st.heap, st.stack) in
      let db_res =
        ITooling.Debugger.run itool.db !(st.itool).code db_st cont s'
      in
      let ((store, heap, stack), cont') = db_res in
      Intermediate ({ st with store; heap; stack }, s' :: cont') $$ DebugEval
    | Block ss -> Intermediate (st, ss @ cont) $$ BlockEval
    | Print e ->
      Log.stdout "%a@." (print_pp st.heap) (eval_expr st e);
      Intermediate (st, cont) $$ PrintEval
    | Return e -> (
      let v = eval_expr st e in
      let f = Call_stack.func st.stack in
      ITooling.Tracer.trace_return lvl f s (st.heap, v);
      let (frame, stack') = Call_stack.pop st.stack in
      match frame with
      | Call_stack.Toplevel _ -> Final v $$ ReturnEval
      | Call_stack.Intermediate (_, restore) ->
        let (store', cont', x) = Call_stack.restore restore in
        ITooling.Tracer.trace_restore (lvl - 1) (Call_stack.func stack');
        Store.set store' x v;
        let st' = { st with store = store'; stack = stack' } in
        Intermediate (st', cont') $$ ReturnEval )
    | Fail e ->
      let v = eval_expr st e in
      Error v $$ FailEval
    | Assert e ->
      let assert_err e = Value.Str (Fmt.str "Assert false: %a" Expr.pp e) in
      let v = eval_bool st e in
      if v then Intermediate (st, cont) $$ AssertEval true
      else Error (assert_err e) $$ AssertEval false
    | Assign (x, e) ->
      let v = eval_expr st e in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignEval
    | AssignCall (x, fe, es) -> (
      let (fn, fvs) = eval_func_expr st fe in
      let vs = fvs @ List.map (eval_expr st) es in
      match ITooling.Monitor.interceptor fn vs es with
      | Some lbl ->
        ITooling.Monitor.set_label itool.mon lbl;
        Intermediate (st, cont)
      | None ->
        let f = get_func p fn fe.at in
        let (stack', store') = prepare_call st f cont x.it vs fe.at in
        let cont' = [ Func.body f ] in
        let db_res = ITooling.Debugger.call itool.db stack' cont' in
        let (stack'', cont'') = db_res in
        let st' = { st with store = store'; stack = stack'' } in
        ITooling.Profiler.count itool.pf `Call;
        ITooling.Tracer.trace_call !(st.itool).code (lvl + 1) f s;
        Intermediate (st', cont'') $$ AssignCallEval f )
    | AssignECall (x, fn, es) ->
      let vs = List.map (eval_expr st) es in
      let v = External.execute p st.store st.heap fn.it vs in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignECallEval
    | AssignNewObj x ->
      let loc = Loc.create () in
      let obj = Object.create () in
      Heap.set st.heap loc obj;
      Store.set st.store x.it (Value.loc loc);
      Intermediate (st, cont) $$ AssignNewObjEval loc
    | AssignObjToList (x, e) ->
      let format_fld (fn, fv) = Value.List [ Str fn; fv ] in
      let (_, obj) = eval_obj st e in
      let v = Value.List (Object.fld_lst obj |> List.map format_fld) in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignObjToListEval
    | AssignObjFields (x, e) ->
      let format_fld (fn, _) = Value.Str fn in
      let (_, obj) = eval_obj st e in
      let v = Value.List (Object.fld_lst obj |> List.map format_fld) in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignObjFieldsEval
    | AssignInObjCheck (x, fe, oe) ->
      let in_obj = function Some _ -> Value.True | None -> Value.False in
      let (loc, obj) = eval_obj st oe in
      let fn = eval_str st fe in
      let v = Object.get obj fn |> in_obj in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ AssignInObjCheckEval (loc, fn)
    | FieldLookup (x, oe, fe) ->
      let fld_val v = Option.value ~default:Value.undefined v in
      let (loc, obj) = eval_obj st oe in
      let fn = eval_str st fe in
      let v = Object.get obj fn |> fld_val in
      Store.set st.store x.it v;
      Intermediate (st, cont) $$ FieldLookupEval (loc, fn)
    | FieldAssign (oe, fe, e) ->
      let (loc, obj) = eval_obj st oe in
      let fn = eval_str st fe in
      let v = eval_expr st e in
      Object.set obj fn v;
      Intermediate (st, cont) $$ FieldAssignEval (loc, fn)
    | FieldDelete (oe, fe) ->
      let (loc, obj) = eval_obj st oe in
      let fn = eval_str st fe in
      Object.delete obj fn;
      Intermediate (st, cont) $$ FieldDeleteEval (loc, fn)
    | If (e, s1, s2) -> (
      let v = eval_bool st e in
      let s2' = Option.value ~default:(Stmt.Skip @?> none) s2 in
      match (v, s1.it, s2'.it) with
      | (true, Block ss, _) | (false, _, Block ss) ->
        let cont' = ss @ ((Stmt.Merge @?> none) :: cont) in
        Intermediate (st, cont') $$ IfEval v
      | (false, _, Skip) -> Intermediate (st, cont) $$ IfEval false
      | (true, _, _) -> Log.fail "expecting if block, but got %a" Stmt.pp s1
      | (false, _, _) -> Log.fail "expecting else block, but got %a" Stmt.pp s2'
      )
    | While (e, s') ->
      let loop = Stmt.If (e, Stmt.Block [ s'; s ] @?> s'.at, None) @?> s.at in
      Intermediate (st, loop :: cont) $$ WhileEval
    | Switch (e, css, dflt) -> (
      let v = eval_expr st e in
      match (Hashtbl.find_opt css v, dflt) with
      | (Some { it = Block ss; _ }, _) | (None, Some { it = Block ss; _ }) ->
        let cont' = ss @ ((Stmt.Merge @?> none) :: cont) in
        Intermediate (st, cont') $$ SwitchEval v
      | (None, None) -> Intermediate (st, cont) $$ SwitchEval v
      | (Some s1', _) ->
        Log.fail "expecting switch block, but got %a" Stmt.pp s1'
      | (None, Some s2') ->
        Log.fail "expecting default block, but got %a" Stmt.pp s2' )

  let eval_small_step_safe (p : Prog.t) (st : st) (s : Stmt.t)
    (cont : Stmt.t list) : return =
    let st' = { st with stack = Call_stack.update st.stack s } in
    try eval_small_step p st' s cont
    with Runtime_error.Error err ->
      Runtime_error.(set_trace st.stack err |> raise)

  let rec small_step_iter (p : Prog.t) (st : st) (cont : Stmt.t list) : return =
    match cont with
    | [] ->
      let fn = Func.name (Call_stack.func st.stack) in
      Runtime_error.(throw ~src:fn.at (MissingReturn fn))
    | s :: cont' -> (
      let return = eval_small_step_safe p st s cont' in
      ITooling.Monitor.eval_small_step !(st.itool).mon;
      match return with
      | Intermediate (st'', cont'') -> small_step_iter p st'' cont''
      | _ -> return )

  let show_exitval (heap : heap) (retval : Value.t) : unit =
    let visited = Hashtbl.create !Base.default_hashtbl_sz in
    let heapval_pp' = rec_print_pp !IConfig.print_depth visited heap in
    if !IConfig.show_exitval then Log.esl "exit value: %a" heapval_pp' retval

  let resolve_exitval (retval : Value.t) : Value.t =
    match retval with
    | Value.List [ Value.False; retval' ] -> retval'
    | Value.List [ Value.True; err ] ->
      Runtime_error.(throw (UncaughtExn (Value.str err)))
    | _ -> Log.fail "unexpected exit value: %a" Value.pp retval

  let result (heap : heap) (itool : ITooling.t ref) (v : Value.t) : IResult.t =
    let metrics = ITooling.Profiler.json !itool.pf in
    let retval = resolve_exitval v in
    (* FIXME: The show_exitval functionality should be on the interpret command *)
    show_exitval heap retval;
    { retval; heap; metrics }

  let eval_instrumented (ientry : IEntry.t) (itool : ITooling.t ref) (p : Prog.t)
    : IResult.t =
    let main = get_func p ientry.main none in
    let st = initial_state ientry.heap itool main in
    ITooling.Tracer.trace_restore (-1) main;
    ITooling.Profiler.start !(st.itool).pf;
    let return = small_step_iter p st [ Func.body main ] in
    ITooling.Profiler.stop !(st.itool).pf st.heap;
    match return with
    | Final v -> result st.heap itool v
    | Error err -> Runtime_error.(throw (Failure (Value.str err)))
    | _ -> Log.fail "unexpected intermediate state"

  let eval_prog code (ientry : IEntry.t) (p : Prog.t) : IResult.t =
    let itool = ref (ITooling.initial_state code) in
    let execute () = eval_instrumented ientry itool p in
    let finally () = ITooling.cleanup !itool in
    Fun.protect ~finally execute
end
