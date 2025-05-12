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

open Core

type vt = Expr.t

type pct = Expr.t

type encoded_pct = Smtml.Expression.t

let rec_size = 1000

module ExprHash = struct
  type t = Expr.t

  let equal (e1 : Expr.t) (e2 : Expr.t) = Expr.equal e1 e2

  let hash (e : Expr.t) = Hashtbl.hash e

  let t_of_sexp e = failwith "Not implemented."

  let sexp_of_t e = failwith "Not implemented"

  let compare (e1 : Expr.t) (e2 : Expr.t) = Hashtbl.hash e1 - Hashtbl.hash e2
end

module Expr_Hashtbl = Hashtbl.Make (ExprHash)

type obj_record =
  { concrete_fields : (String.t, Expr.t option) Hashtbl.t
  ; symbolic_field : (Expr.t * Expr.t option) option
  }

type t = obj_record list

let create_obj_record () : obj_record =
  { concrete_fields = Hashtbl.create (module String); symbolic_field = None }

let create () : t = [ create_obj_record () ]

let clone (o : t) : t =
  let new_rec = create_obj_record () in
  new_rec :: o

let obj_record_to_string (o_r : obj_record) (printer : Expr.t -> string) :
  string =
  let aux e = match e with Some v -> printer v | None -> "deleted" in

  let str_obj =
    Hashtbl.fold o_r.concrete_fields ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
      (if String.(ac <> "{ ") then ac ^ ", " else ac)
      ^ Printf.sprintf "\"%s\": %s" n (aux v) )
    ^ "|"
  in
  match o_r.symbolic_field with
  | None -> str_obj ^ " }"
  | Some (key, data) ->
    str_obj ^ Printf.sprintf "\"%s\": %s" (printer key) (aux data) ^ " }"

let to_string (o : t) (printer : Expr.t -> string) : string =
  List.fold o ~init:"" ~f:(fun acc o_r ->
    acc ^ obj_record_to_string o_r printer ^ "@" )

let record_has_concrete_key (o : obj_record) (key : string) : bool =
  let res = Hashtbl.find o.concrete_fields key in
  match res with Some _ -> true | None -> false

let record_concrete_list (o : obj_record) : (Expr.t * Expr.t option) list =
  let s_l = Hashtbl.to_alist o.concrete_fields in
  List.map s_l ~f:(fun (k, v) -> (Expr.Val (Val.Str k), v))

let record_concrete_list2 (o : obj_record) : (Expr.t * Expr.t) list =
  let s_l = Hashtbl.to_alist o.concrete_fields in
  List.fold s_l ~init:[] ~f:(fun acc (k, v) ->
    match v with Some v' -> acc @ [ (Expr.Val (Val.Str k), v') ] | None -> acc )

let record_concrete_keys (o : obj_record) : Expr.t list =
  let s_l = Hashtbl.keys o.concrete_fields in
  List.map s_l ~f:(fun k -> Expr.Val (Val.Str k))

let concrete_to_list (o : t) : (Expr.t * Expr.t option) list =
  List.fold o ~init:[] ~f:(fun accum o_r -> accum @ record_concrete_list o_r)

let mk_eq e1 e2 = Expr.BinOpt (Operators.Eq, e1, e2)

let mk_ite e1 e2 e3 = Expr.TriOpt (Operators.Conditional, e1, e2, e3)

let mk_or e1 e2 =
  if Expr.equal e1 (Expr.Val (Val.Bool false)) then e2
  else if Expr.equal e2 (Expr.Val (Val.Bool false)) then e1
  else Expr.BinOpt (Operators.Log_Or, e1, e2)

let mk_not e1 = Expr.UnOpt (Operators.Not, e1)

let is_key_possible (k1 : Expr.t) (k2 : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) : bool =
  let eq0 = mk_eq k1 k2 in
  let eq = Reducer.reduce_expr store eq0 in
  Batch.check solver (eq :: pc)

let create_not_pct (l : (pct * Expr.t) list) (key : pct) (store : S_store.t) :
  encoded_pct list =
  List.fold l ~init:[] ~f:(fun acc (pc, _) ->
    let ne = Expr.UnOpt (Operators.Not, mk_eq key pc) in
    let expr = Reducer.reduce_expr store ne in
    expr :: acc )

let create_object (o : t) (k1 : pct) (k2 : pct) (store : S_store.t) :
  t * encoded_pct list =
  let o' = clone o in
  let eq = Reducer.reduce_expr store (mk_eq k1 k2) in
  (o', [ eq ])

let mk_ite_expr (prop : Expr.t) (conds : (Expr.t * Expr.t) list list)
  (default_val : Expr.t) (pc : encoded_pct list) (solver : Batch.t)
  (store : S_store.t) : Expr.t =
  List.fold ~init:default_val
    ~f:(fun acc_ite l ->
      List.fold l ~init:acc_ite ~f:(fun acc (cond, data) ->
        let eq = Reducer.reduce_expr store cond in
        let ite = Reducer.reduce_expr store (mk_ite eq data acc) in
        ite ) )
    conds

let same_type (e1 : Expr.t) (e2 : Expr.t) : bool =
  let t1 = Sval_typing.type_of e1 in
  let t2 = Sval_typing.type_of e2 in

  match (t1, t2) with (Some t1, Some t2) -> Type.( = ) t1 t2 | _ -> false

let rec mk_ite_expr_aux (accs : (Expr.t * Expr.t option) list) (acc_pc : Expr.t)
  (default_val : Expr.t) (default_pc : Expr.t)
  (conds : (Expr.t * Expr.t) list list) (solver : Batch.t) (store : S_store.t) :
  (Expr.t * Expr.t option) list * Expr.t =
  let false_e = Expr.Val (Val.Bool false) in
  let undef = Expr.Val (App (`Op "symbol", [ Str "undefined" ])) in
  let (ite, new_pc, new_default) =
    List.fold
      ~init:(default_val, default_pc, undef)
      ~f:(fun acc_ite l ->
        List.fold l ~init:acc_ite
          ~f:(fun (acc_val, acc_pc, new_default) (cond, data) ->
          if same_type default_val data then
            let eq = Reducer.reduce_expr store cond in
            let p = if Expr.equal false_e acc_pc then eq else mk_or acc_pc eq in
            let ite = Reducer.reduce_expr store (mk_ite eq data acc_val) in
            (ite, p, new_default)
          else
            ( acc_val
            , acc_pc
            , if Expr.equal undef new_default then data else new_default ) ) )
      conds
  in

  if Expr.equal undef new_default then
    ((ite, Some new_pc) :: accs, mk_or acc_pc new_pc)
  else
    mk_ite_expr_aux
      ((ite, Some new_pc) :: accs)
      (mk_or acc_pc new_pc) new_default false_e conds solver store

let mk_ite_expr_get (prop : Expr.t) (conds : (Expr.t * Expr.t) list list)
  (default_val : Expr.t * Expr.t option) (pc : encoded_pct list)
  (solver : Batch.t) (store : S_store.t) : (Expr.t * pct option) list =
  let undef = Expr.Val (App (`Op "symbol", [ Str "undefined" ])) in
  let false_e = Expr.Val (Val.Bool false) in
  let (default_val, default_pc) = default_val in

  if List.length conds = 0 then [ (default_val, None) ]
  else
    let (default_val, conds, default_pc, undef_possible) =
      if Expr.equal undef default_val then
        let lst = List.hd_exn conds in
        let (cond, data) = List.hd_exn lst in
        ( data
        , List.tl_exn lst :: List.tl_exn conds
        , Reducer.reduce_expr store cond
        , true )
      else
        let default_pc' =
          match default_pc with Some p -> p | None -> false_e
        in
        (default_val, conds, default_pc', false)
    in

    let (l, new_pc) =
      mk_ite_expr_aux [] (Expr.Val (Val.Bool false)) default_val default_pc
        conds solver store
    in

    let not_new = mk_not new_pc in

    if undef_possible then (undef, Some not_new) :: l else l

let get_prop_rec (o_rec : obj_record) (prop : Expr.t)
  (get_val : Expr.t option -> Expr.t) (solver : Batch.t) (pc : encoded_pct list)
  (store : S_store.t) :
  (Expr.t * Expr.t) list * (Expr.t option * Expr.t option) option =
  let open Expr in
  let open Val in
  let (ret_lst, ret_e) =
    match o_rec.symbolic_field with
    | None -> ([], None)
    | Some (prop', v) when Expr.equal prop' prop -> ([], Some (v, None))
    | Some (prop', v) ->
      let eq = Reducer.reduce_expr store (mk_eq prop' prop) in
      if is_key_possible prop' prop solver (eq :: pc) store then
        ([ (eq, get_val v) ], None)
      else ([], None)
  in
  let (ret_lst, ret_e) =
    match prop with
    | Val (Str p) when Hashtbl.mem o_rec.concrete_fields p ->
      (ret_lst, Some (Hashtbl.find_exn o_rec.concrete_fields p, None))
    | Val (Str p) -> (ret_lst, ret_e)
    | _ -> (
      match ret_e with
      | Some e -> (ret_lst, ret_e)
      | None ->
        ( Hashtbl.fold o_rec.concrete_fields ~init:ret_lst
            ~f:(fun ~key:k ~data:v acc ->
            if is_key_possible (Val (Str k)) prop solver pc store then
              let eq = Reducer.reduce_expr store (mk_eq (Val (Str k)) prop) in
              let ret = (eq, get_val v) in
              ret :: acc
            else acc )
        , ret_e ) )
  in
  (ret_lst, ret_e)

let rec get_prop_aux ?(default_val = Expr.Val (Val.Bool false)) (o : t)
  (prop : Expr.t) (get_val : Expr.t option -> Expr.t)
  (lst : (Expr.t * Expr.t) list list) (solver : Batch.t) (pc : encoded_pct list)
  (store : S_store.t) : (Expr.t * Expr.t) list list * (Expr.t * Expr.t option) =
  match o with
  | [] -> (lst, (default_val, None))
  | o_rec :: o_rest -> (
    let (lst', final_val) = get_prop_rec o_rec prop get_val solver pc store in
    match (lst', final_val) with
    | ([], Some e) ->
      let (v, pc') = e in
      (lst, (get_val v, pc'))
    | (lst', Some e) ->
      let (v, pc') = e in
      (lst' :: lst, (get_val v, pc'))
    | ([], None) ->
      get_prop_aux ~default_val o_rest prop get_val lst solver pc store
    | (((cond, v) :: rest as lst'), None) ->
      let false_e = Expr.Val (Val.Bool false) in
      let new_pc =
        List.fold lst' ~init:false_e ~f:(fun acc (eq, v) ->
          if Expr.equal false_e acc then eq else mk_or acc eq )
      in
      let not_new = mk_not new_pc in

      if Batch.check solver (not_new :: pc) then
        get_prop_aux ~default_val o_rest prop get_val (lst' :: lst) solver
          (not_new :: pc) store
      (* Este caso existe? *)
      (* else if List.length lst + List.length lst' = 0 then ([], default_val) *)
        else (rest :: lst, (v, Some cond)) )

let get_prop ?(default_val = Expr.Val (Val.Bool false)) (o : t) (prop : Expr.t)
  (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t)
  (get_val : Expr.t option -> Expr.t) : Expr.t =
  let (conds, (last_val, _)) =
    get_prop_aux ~default_val o prop get_val [] solver pc store
  in
  let e = mk_ite_expr prop conds last_val pc solver store in
  e

let has_field (o : t) (k : Expr.t) (solver : Batch.t) (pc : encoded_pct list)
  (store : S_store.t) : Expr.t =
  get_prop o k solver pc store (fun v ->
    match v with
    | Some v -> Expr.Val (Val.Bool true)
    | None -> Expr.Val (Val.Bool false) )

let set (o : t) (key : vt) (data : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) : t * encoded_pct list =
  match key with
  | Expr.Val (Val.Str key_s) ->
    let o_r = List.hd_exn o in
    let o =
      if Hashtbl.length o_r.concrete_fields >= rec_size then (
        let new_or = create_obj_record () in
        Hashtbl.set new_or.concrete_fields ~key:key_s ~data:(Some data);
        new_or :: o )
      else
        let _ = Hashtbl.set o_r.concrete_fields ~key:key_s ~data:(Some data) in
        o
    in
    (o, [])
  | _ ->
    let o_r = List.hd_exn o in
    let tl = List.tl_exn o in
    let new_written = { o_r with symbolic_field = Some (key, Some data) } in
    let new_rec = create_obj_record () in
    (new_rec :: new_written :: tl, [])

let get (o : t) (key : vt) (solver : Batch.t) (pc : encoded_pct list)
  (store : S_store.t) : (Expr.t * pct option) list =
  let undef = Expr.Val (App (`Op "symbol", [ Str "undefined" ])) in
  let get_val v = match v with Some v -> v | _ -> undef in
  let (conds, last_val) =
    get_prop_aux ~default_val:undef o key get_val [] solver pc store
  in
  let res = mk_ite_expr_get key conds last_val pc solver store in
  res

let delete (o : t) (key : Expr.t) (solver : Batch.t) (pc : encoded_pct list)
  (store : S_store.t) : (t * encoded_pct list) list =
  match key with
  | Expr.Val (Val.Str key_s) ->
    let o_r = List.hd_exn o in
    Hashtbl.set o_r.concrete_fields ~key:key_s ~data:None;
    [ (o, []) ]
  | _ ->
    let o_r = List.hd_exn o in
    let tl = List.tl_exn o in
    let new_written = { o_r with symbolic_field = Some (key, None) } in
    let new_rec = create_obj_record () in
    [ (new_rec :: new_written :: tl, []) ]

(* let to_json (o : 'a t) (printer : 'a -> string) : string =
   let str_obj =
     Hashtbl.fold o ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
         (if String.(ac <> "{ ") then ac ^ ", " else ac)
         ^ Printf.sprintf "\"%s\": %s" n (printer v))
   in
   str_obj ^ " }" *)

let to_list (o : t) : (Expr.t * Expr.t) list =
  let (c, s) =
    List.fold o ~init:([], []) ~f:(fun (concrete, symb) o_r ->
      let s =
        match o_r.symbolic_field with
        | None -> symb
        | Some (k, Some v') -> (k, v') :: symb
        | _ -> symb
      in
      (concrete @ record_concrete_list2 o_r, s) )
  in
  c @ s

let get_fields (o : t) : Expr.t list =
  let (c, s) =
    List.fold o ~init:([], []) ~f:(fun (concrete, symb) o_r ->
      let s =
        match o_r.symbolic_field with
        | None -> symb
        | Some (key, data) -> key :: symb
      in
      (concrete @ record_concrete_keys o_r, s) )
  in
  c @ s
