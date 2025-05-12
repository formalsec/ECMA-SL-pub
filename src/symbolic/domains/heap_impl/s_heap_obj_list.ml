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
module Object = S_object_list

type encoded_pct = Smtml.Expression.t

type obj = Object.t

type t =
  { parent : t option
  ; map : (Loc.t, obj) Hashtbl.t
  }

let create () : t = { parent = None; map = Hashtbl.create (module String) }

let clone (h : t) : t =
  { parent = Some h; map = Hashtbl.create (module String) }

let insert (h : t) (obj : obj) : Loc.t =
  let loc = Loc.newloc () in
  Hashtbl.set h.map ~key:loc ~data:obj;
  loc

let remove (h : t) (l : Loc.t) : unit = Hashtbl.remove h.map l

let set (h : t) (key : Loc.t) (data : obj) : unit = Hashtbl.set h.map ~key ~data

let rec get ?(setVal = true) (h : t) (l : Loc.t) : obj option =
  let result = Hashtbl.find h.map l in
  match result with
  | Some o -> result
  | None -> (
    let obj = Option.bind h.parent ~f:(fun h -> get h l ~setVal:false) in
    match obj with
    | Some o ->
      if setVal then (
        let o' = Object.clone o in
        set h l o';
        Some o' )
      else Some o
    | None -> None )

let mk_ite (e1 : Expr.t) (e2 : Expr.t) (e3 : Expr.t) : Expr.t =
  Expr.TriOpt (Operators.Conditional, e1, e2, e3)

let mk_not (e : Expr.t) : Expr.t = Expr.UnOpt (Operators.Not, e)

let mk_bool (b : bool) : Expr.t = Expr.Val (Val.Bool false)

let apply_op_get (h : t) (loc : Expr.t) (cond : Expr.t) (left : Expr.t)
  (right : Expr.t) (solver : Batch.t)
  (op : Expr.t -> encoded_pct list -> Expr.t) (pc : encoded_pct list)
  (store : S_store.t) : 'a =
  let pc_left = cond :: pc in
  let pc_right = mk_not cond :: pc in
  let cs = Batch.check solver in

  match (cs pc_left, cs pc_right) with
  | (true, true) ->
    let vl = op left pc_left in
    let vr = op right pc_right in
    mk_ite cond vl vr
  | (true, false) -> op left pc_left
  | (false, true) -> op right pc_right
  | _ -> failwith "Apply op error."

let apply_op_set (h : t) (loc : Expr.t) (cond : Expr.t) (left : Expr.t)
  (right : Expr.t) (solver : Batch.t)
  (op :
       Expr.t
    -> encoded_pct list
    -> encoded_pct option
    -> t
    -> (t * encoded_pct list) list ) (pc : encoded_pct list) (store : S_store.t)
  : (t * encoded_pct list) list =
  let not_cond = mk_not cond in
  let pc_l = cond :: pc in
  let pc_r = not_cond :: pc in
  let cs = Batch.check solver in

  match (cs pc_l, cs pc_r) with
  | (true, true) ->
    op left pc_l (Some cond) (clone h) @ op right pc_r (Some not_cond) (clone h)
  | (true, false) -> op left pc_l (Some cond) (clone h)
  | (false, true) -> op right pc_r (Some not_cond) (clone h)
  | _ -> failwith "No path is valid in Set."

let rec assign_obj_fields (h : t) (loc : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) : 'a =
  match loc with
  | Expr.Val (Val.Loc l) -> (
    let obj = get h l in
    match obj with
    | None -> failwith "Object not found."
    | Some o -> Expr.NOpt (Operators.ListExpr, Object.get_fields o) )
  | Expr.TriOpt (Operators.Conditional, cond, left, right) ->
    let op l pc = assign_obj_fields h l solver pc store in
    apply_op_get h loc cond left right solver op pc store
  | Expr.Val (App (`Op "symbol", [ Str "undefined" ])) ->
    invalid_arg ("Invalid location in assign_obj_fields: " ^ Expr.str loc)
  | _ -> assert false

let rec assign_obj_to_list (h : t) (loc : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) : 'a =
  match loc with
  | Expr.Val (Val.Loc l) -> (
    let obj = get h l in
    match obj with
    | None -> failwith "Object not found."
    | Some o ->
      let ret =
        Expr.NOpt
          ( Operators.ListExpr
          , List.map (Object.to_list o) ~f:(fun (f, v) ->
              Expr.NOpt (Operators.TupleExpr, [ f; v ]) ) )
      in
      ret )
  | Expr.TriOpt (Operators.Conditional, cond, left, right) ->
    let op l pc = assign_obj_to_list h l solver pc store in
    apply_op_get h loc cond left right solver op pc store
  | Expr.Val (App (`Op "symbol", [ Str "undefined" ])) ->
    invalid_arg ("Invalid location in assign_obj_to_list: " ^ Expr.str loc)
  | _ -> assert false

let rec has_field_aux (h : t) (loc : Expr.t) (field : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) : Expr.t =
  match loc with
  | Expr.Val (Val.Loc l) ->
    Option.value_map (get h l ~setVal:false) ~default:(mk_bool false)
      ~f:(fun o -> Object.has_field o field solver pc store )
  | Expr.TriOpt (Operators.Conditional, cond, left, right) ->
    let op l pc = has_field_aux h l field solver pc store in
    apply_op_get h loc cond left right solver op pc store
  | Expr.Val (App (`Op "symbol", [ Str "undefined" ])) ->
    invalid_arg ("Invalid location in has_field: " ^ Expr.str loc)
  | _ -> assert false

let has_field (h : t) (loc : Expr.t) (field : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) :
  (t * encoded_pct list * Expr.t) list =
  [ (h, [], has_field_aux h loc field solver pc store) ]

let rec get_field_aux (heap : t) (loc : Expr.t) (field : Expr.t)
  (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) : 'a =
  match loc with
  | Expr.Val (Val.Loc l) -> (
    let obj = get heap l ~setVal:false in
    match obj with
    | None -> failwith "Object not found."
    | Some o -> Object.get o field solver pc store )
  | Expr.TriOpt (Operators.Conditional, cond, left, right) ->
    let op l pc = get_field_aux heap l field solver pc store in
    apply_op_get heap loc cond left right solver op pc store
  | Expr.Val (App (`Op "symbol", [ Str "undefined" ])) ->
    invalid_arg ("Invalid location in get field: " ^ Expr.str loc)
  | _ -> assert false

let get_field (heap : t) (loc : Expr.t) (field : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) :
  (t * encoded_pct list * Expr.t option) list =
  [ (heap, [], Some (get_field_aux heap loc field solver pc store)) ]

let set_field_exec (heap : t) (loc : Loc.t) (field : Expr.t) (v : 'a)
  (solver : Batch.t) (pc : encoded_pct list)
  (encoded_guard : encoded_pct option) (store : S_store.t) :
  (t * encoded_pct list) list =
  let obj = get heap loc in
  match obj with
  | None -> failwith ("set Return is never none. loc: " ^ loc)
  | Some o ->
    let l = Object.set o field v solver pc store in
    List.map l ~f:(fun (o, pc') ->
      set heap loc o;
      (heap, pc') )

let rec set_field_aux ?(encoded_guard = None) (heap : t) (loc : Expr.t)
  (field : Expr.t) (v : 'a) (solver : Batch.t) (pc : encoded_pct list)
  (store : S_store.t) : (t * encoded_pct list) list =
  match loc with
  | Expr.Val (Val.Loc l) ->
    set_field_exec heap l field v solver pc encoded_guard store
  | Expr.TriOpt (Operators.Conditional, cond, left, right) ->
    let op l pc guard h =
      set_field_aux ~encoded_guard:guard h l field v solver pc store
    in
    apply_op_set heap loc cond left right solver op pc store
  | Expr.Val (App (`Op "symbol", [ Str "undefined" ])) ->
    invalid_arg ("Invalid location in set field: " ^ Expr.str loc)
  | _ -> assert false

let set_field (heap : t) (loc : Expr.t) (field : Expr.t) (v : 'a)
  (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t) :
  (t * encoded_pct list) list =
  set_field_aux heap loc field v solver pc store

let delete_field_exec (heap : t) (loc : Loc.t) (field : Expr.t)
  (solver : Batch.t) (pc : encoded_pct list)
  (encoded_guard : encoded_pct option) (store : S_store.t) :
  (t * encoded_pct list) list =
  let obj = get heap loc in
  match obj with
  | None -> failwith ("Delete obj is never none. loc: " ^ loc)
  | Some o ->
    let l = Object.delete o field solver pc store in
    List.map l ~f:(fun (o, pc') ->
      set heap loc o;
      (heap, pc') )

let rec delete_field_aux ?(encoded_guard = None) (heap : t) (loc : Expr.t)
  (field : Expr.t) (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t)
  : (t * encoded_pct list) list =
  match loc with
  | Expr.Val (Val.Loc l) ->
    delete_field_exec heap l field solver pc encoded_guard store
  | Expr.TriOpt (Operators.Conditional, cond, left, right) ->
    let op l pc guard h =
      delete_field_aux ~encoded_guard:guard h l field solver pc store
    in
    apply_op_set heap loc cond left right solver op pc store
  | Expr.Val (App (`Op "symbol", [ Str "undefined" ])) ->
    invalid_arg ("Invalid location in delete field: " ^ Expr.str loc)
  | _ -> assert false

let delete_field (heap : t) (loc : Expr.t) (field : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) : (t * encoded_pct list) list =
  delete_field_aux heap loc field solver pc store
(* let to_string (h : 'a t) (pp : 'a -> string) : string =
    "{ "
    ^ String.concat ~sep:", "
        (Hashtbl.fold h.map ~init:[] ~f:(fun ~key:n ~data:v acc ->
            Printf.sprintf "%s: %s" (Loc.str n) (S_object.to_string v pp) :: acc))
    ^ " }" *)

(* let to_string_with_glob (h : 'a t) (pp : 'a -> string) : string =
    let glob =
      Hashtbl.fold h.map ~init:None ~f:(fun ~key:_ ~data:obj acc ->
          match acc with
          | Some _ -> acc
          (* Keep this in sync with Compiler.ml function *)
          (* "compile_gvar" and "compile_glob_assign" *)
          | None -> S_object.get_concrete_field obj Common.global_var_compiled)
    in
    match glob with
    | Some l ->
        Printf.sprintf "{ \"heap\": %s, \"global\": %s }" (to_string h pp)
          (Val.str l)
    | None ->
        raise
          (Failure
            "Couldn't find the Object that contains only one property, named \
              \"global\".") *)
