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
module Object = S_object_list_undef

type encoded_pct = Smtml.Expression.t

type obj = Object.t

type t =
  { parent : t option
  ; map : (Loc.t, obj) Hashtbl.t
  }

let caching = false

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

let mk_and (e1 : Expr.t) (e2 : Expr.t) : Expr.t =
  Expr.BinOpt (Operators.Log_And, e1, e2)

let mk_or (e1 : Expr.t) (e2 : Expr.t) : Expr.t =
  Expr.BinOpt (Operators.Log_Or, e1, e2)

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
    Option.value_map (get h l ~setVal:caching) ~default:(mk_bool false)
      ~f:(fun o -> Object.has_field o field solver pc store )
  | Expr.TriOpt (Operators.Conditional, cond, left, right) ->
    let op l pc = has_field_aux h l field solver pc store in
    apply_op_get h loc cond left right solver op pc store
  | Expr.Val (App (`Op "symbol", [ Str "undefined" ])) ->
    invalid_arg ("Invalid location in has_field: " ^ Expr.str loc)
    (* mk_bool false *)
  | _ -> assert false

let has_field (h : t) (loc : Expr.t) (field : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) :
  (t * encoded_pct list * Expr.t) list =
  [ (h, [], has_field_aux h loc field solver pc store) ]

let getKey (tp : Type.t option) : int =
  match tp with
  | Some Type.IntType -> 1
  | Some Type.FltType -> 2
  | Some Type.BoolType -> 3
  | Some Type.StrType -> 4
  | Some Type.LocType -> 5
  | Some Type.ListType -> 6
  | Some Type.TypeType -> 7
  | Some Type.TupleType -> 8
  | Some Type.NullType -> 9
  | Some Type.CurryType -> 10
  | Some Type.ArrayType -> 11
  | Some Type.SymbolType -> 12
  | None -> -1

let rec get_field_aux ?(guard = None) (heap : t) (loc : Expr.t) (field : Expr.t)
  (solver : Batch.t) (pc : encoded_pct list) (store : S_store.t)
  (acc : (int, (Expr.t * Expr.t option * Expr.t option) list) Hashtbl.t) :
  (int, (Expr.t * Expr.t option * Expr.t option) list) Hashtbl.t =
  match loc with
  | Expr.Val (Val.Loc l) -> (
    let obj = get heap l ~setVal:caching in
    match obj with
    | None -> failwith "Object not found."
    | Some o ->
      let l = Object.get o field solver pc store in

      List.fold l ~init:acc ~f:(fun acc (v, pc') ->
        let k = getKey (Sval_typing.type_of v) in

        let lst = Hashtbl.find acc k in
        let lst = match lst with None -> [] | Some lst -> lst in

        let lst =
          match (pc', guard) with
          | (Some p, Some g) -> (v, Some (mk_and g p), guard) :: lst
          | (None, Some g) -> (v, Some g, guard) :: lst
          | (Some p, None) -> (v, Some p, guard) :: lst
          | (None, None) -> (v, None, guard) :: lst
        in
        Hashtbl.set acc ~key:k ~data:lst;
        acc ) )
  | Expr.TriOpt (Operators.Conditional, cond, left, right) -> (
    let op l pc guard = get_field_aux ~guard heap l field solver pc store in
    let not_cond = mk_not cond in
    let pc_left = cond :: pc in
    let pc_right = not_cond :: pc in
    let cs = Batch.check solver in
    match (cs pc_left, cs pc_right) with
    | (true, true) ->
      let acc' = op left pc_left (Some cond) acc in
      let acc' = op right pc_right (Some not_cond) acc' in
      acc'
    | (true, false) -> op left pc_left (Some cond) acc
    | (false, true) -> op right pc_right (Some not_cond) acc
    | _ -> failwith "Apply op error." )
  | Expr.Val (App (`Op "symbol", [ Str "undefined" ])) ->
    invalid_arg ("Invalid location in getr_field: " ^ Expr.str loc)
  | _ -> assert false

let or_aux (e1 : Expr.t) (e2 : Expr.t option) : Expr.t =
  match e2 with Some e2 -> mk_or e1 e2 | None -> e1

let get_field (heap : t) (loc : Expr.t) (field : Expr.t) (solver : Batch.t)
  (pc : encoded_pct list) (store : S_store.t) :
  (t * encoded_pct list * Expr.t option) list =
  let branch_table = Hashtbl.create (module Int) in
  let branch_table =
    get_field_aux heap loc field solver pc store branch_table
  in
  let undef = Expr.Val (App (`Op "symbol", [ Str "undefined" ])) in
  let f_e = mk_bool false in
  if Hashtbl.length branch_table > 1 then
    Hashtbl.fold branch_table ~init:[] ~f:(fun ~key ~data acc ->
      let (ite, pc') =
        List.fold data ~init:(undef, f_e)
          ~f:(fun (ite, new_pc) (v, pc', guard) ->
          let new_pc' =
            if Expr.equal f_e new_pc then Option.value_exn pc'
            else or_aux new_pc pc'
          in

          let ite' =
            match guard with
            | Some g -> if Expr.equal undef ite then v else mk_ite g v ite
            | None -> v
          in
          (ite', new_pc') )
      in
      (clone heap, [ pc' ], Some ite) :: acc )
  else
    Hashtbl.fold branch_table ~init:[] ~f:(fun ~key ~data acc ->
      let (ite, pc') =
        List.fold data ~init:(undef, Some f_e)
          ~f:(fun (ite, new_pc) (v, pc', guard) -> (v, pc') )
      in
      let pc' = match pc' with Some p -> [ p ] | None -> [] in
      (heap, pc', Some ite) :: acc )

(* if List.length branch_list > 1 then
     List.map branch_list ~f:(fun (v, pc') ->
       let p = match pc' with | Some p -> [ p] | None -> [] in
       (clone heap, p, Some v)
   )
   else
     let v, pc' = List.hd_exn branch_list in
     let p = match pc' with | Some p -> [ p] | None -> [] in
     [(heap, p, Some v)] *)

(* let undef = Expr.Val (App (`Op "symbol", [Str "undefined"])) in *)
(* let pc_undef = List.fold acc_undef ~init:(false_e) ~f:(fun acc (_, pc') ->
     if Expr.equal acc false_e then
       pc'
     else
       mk_or pc' acc
   ) in
   let undef = Expr.Val (App (`Op "symbol", [Str "undefined"])) in
   let v_ite =
     List.fold acc_conc ~init:(undef)
       ~f:(fun acc_v (v, pc', guard) ->
         let acc_v =
           match guard with
           | None -> v
           | Some guard ->
             if Expr.equal acc_v undef || Expr.equal acc_v v then v
             else
               mk_ite guard v acc_v
         in acc_v
       )
   in
   if Expr.equal (pc_undef) false_e then
     (* let _ = Printf.printf ("i should get here conc only %s\n") (Expr.str v_ite) in *)
     [(heap, [], Some v_ite)]
   else
     (* let _ = Printf.printf ("i shouldnt get here both %s undef\n") (Expr.str v_ite) in *)
     [
       (clone heap, [ (mk_not pc_undef)], Some v_ite);
       (clone heap, [ pc_undef], None)
     ] *)

let set_field_exec (heap : t) (loc : Loc.t) (field : Expr.t) (v : 'a)
  (solver : Batch.t) (pc : encoded_pct list)
  (encoded_guard : encoded_pct option) (store : S_store.t) :
  (t * encoded_pct list) list =
  let obj = get heap loc in
  match obj with
  | None -> failwith ("set Return is never none. loc: " ^ loc)
  | Some o ->
    let (o, pc') = Object.set o field v solver pc store in
    set heap loc o;
    [ (heap, pc') ]
(* let l = Object.set o field v solver pc store in
   List.map l ~f:(fun (o, pc') ->
       set heap loc o;
       (heap, pc')) *)

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
    invalid_arg ("Invalid location in set_field: " ^ Expr.str loc)
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
    invalid_arg ("Invalid location in delete_field: " ^ Expr.str loc)
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
