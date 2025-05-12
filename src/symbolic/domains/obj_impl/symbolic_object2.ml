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
module V = Symbolic_value.M
module Reducer = Value_reducer
module Translator = Value_translator

module M :
  Object_intf.S2 with type value = V.value and type value2 = Smtml.Expr.t =
struct
  type value = V.value

  type value2 = Smtml.Expr.t

  type t =
    { concrete_fields : (string, value) Hashtbl.t
    ; symbolic_fields : (value, value) Hashtbl.t
    }

  let create () : t =
    { concrete_fields = Hashtbl.create 16; symbolic_fields = Hashtbl.create 16 }

  let clone (o : t) : t =
    { concrete_fields = Hashtbl.copy o.concrete_fields
    ; symbolic_fields = Hashtbl.copy o.symbolic_fields
    }

  (* let to_string (o : t) (printer : Expr.t -> string) : string = *)
  (*   let str_obj = *)
  (*     Hashtbl.fold o.concrete_fields ~init:"{ " ~f:(fun ~key:n ~data:v ac -> *)
  (*         (if String.(ac <> "{ ") then ac ^ ", " else ac) *)
  (*         ^ Printf.sprintf "\"%s\": %s" n (printer v) ) *)
  (*     ^ "|||" *)
  (*   in *)
  (*   let str_obj = *)
  (*     List.fold_left (Expr_Hashtbl.to_alist o.symbolic_fields) *)
  (*       ~init:(str_obj ^ ", ") ~f:(fun acc (key, data) -> *)
  (*         acc ^ Printf.sprintf "\"$symb_%s\": %s, " (Expr.str key) (printer data) ) *)
  (*   in *)
  (*   str_obj ^ " }" *)

  let is_empty o =
    Hashtbl.(length o.concrete_fields = 0 && length o.symbolic_fields = 0)

  let set_symbolic_field (o : t) (key : value) (data : value) : unit =
    Hashtbl.replace o.symbolic_fields key data

  let set_concrete_field (o : t) (key : value) (data : value) : unit =
    match key with
    | V.Val (Val.Str field) -> Hashtbl.replace o.concrete_fields field data
    | _ -> Log.stderr "bad field: %a" V.pp key

  let has_concrete_key (o : t) (key : string) : bool =
    Hashtbl.mem o.concrete_fields key

  let concrete_to_list (o : t) : (value * value) list =
    Hashtbl.fold
      (fun k v acc -> (V.Val (Val.Str k), v) :: acc)
      o.concrete_fields []

  let get_symbolic_field (o : t) (key : value) : value option =
    Hashtbl.find_opt o.symbolic_fields key

  let mk_eq e1 e2 = V.BinOpt (Operator.Eq, e1, e2)

  let create_not_pct (l : (value * value) list) (key : value) : value2 list =
    List.fold_left
      (fun acc (pc, _) ->
        let ne = V.UnOpt (Operator.LogicalNot, mk_eq key pc) in
        ne :: acc )
      [] l

  let create_object (o : t) (k1 : value) (k2 : value) : t * value2 list =
    let o' = clone o in
    let eq = mk_eq k1 k2 in
    (o', [ eq ])

  let is_key_possible (k1 : value) (k2 : value) (solver : Solver.t)
    (pc : value2 list) : bool =
    let eq = mk_eq k1 k2 in
    `Sat = Solver.check solver (eq :: pc)

  let get_possible_fields_concrete (o : t) (key : value) (solver : Solver.t)
    (pc : value2 list) : (value * value) list =
    Hashtbl.fold
      (fun k d acc ->
        let k' = V.Val (Val.Str k) in
        if is_key_possible key k' solver pc then (k', d) :: acc else acc )
      o.concrete_fields []

  let get_possible_fields_symbolic (o : t) (key : value) (solver : Solver.t)
    (pc : value2 list) : (value * value) list =
    Hashtbl.fold
      (fun k d acc ->
        if is_key_possible key k solver pc then (k, d) :: acc else acc )
      o.symbolic_fields []

  let mk_ite e1 e2 e3 = V.TriOpt (Operator.Conditional, e1, e2, e3)

  let is_val = function V.Val _ -> true | _ -> false

  let has_field (o : t) (k : value) : value =
    let open Val in
    if
      Hashtbl.length o.concrete_fields = 0
      && Hashtbl.length o.symbolic_fields = 0
    then Val (Bool false)
    else if is_val k then
      match k with
      | Val (Str s) ->
        if Hashtbl.mem o.concrete_fields s then Val (Bool true)
        else
          Hashtbl.fold
            (fun key _ acc -> mk_ite (mk_eq k key) (Val (Bool true)) acc)
            o.symbolic_fields (Val (Bool false))
      | _ -> failwith "impossible"
    else
      let v0 =
        Hashtbl.fold
          (fun key _ acc -> mk_ite (mk_eq k key) (Val (Bool true)) acc)
          o.symbolic_fields (Val (Bool false))
      in
      Hashtbl.fold
        (fun key _ acc -> mk_ite (mk_eq k (Val (Str key))) (Val (Bool true)) acc)
        o.concrete_fields v0

  let set (o : t) ~(key : value) ~(data : value) (solver : Solver.t)
    (pc : value2 list) : (t * value2 list) list =
    match key with
    | V.Val (Val.Str s) ->
      if has_concrete_key o s || Hashtbl.length o.symbolic_fields = 0 then
        let _ = set_concrete_field o key data in
        [ (o, []) ]
      else
        let lst = get_possible_fields_symbolic o key solver pc in
        (* create an object for each possible equality *)
        let rets =
          List.map
            (fun (k, _) ->
              let (o', pc') = create_object o key k in
              set_concrete_field o' key data;
              Hashtbl.remove o'.symbolic_fields k;
              (o', pc') )
            lst
        in
        (* update current object with the condition of not
           being equal to any of the existing fields *)
        let new_pc = create_not_pct lst key in
        if `Sat = Solver.check solver (new_pc @ pc) then (
          let o' = clone o in
          Hashtbl.replace o'.concrete_fields s data;
          (o', new_pc) :: rets )
        else rets
    | _ ->
      let temp =
        Hashtbl.length o.concrete_fields + Hashtbl.length o.symbolic_fields
      in
      if temp = 0 then (
        set_symbolic_field o key data;
        [ (o, []) ] )
      else
        let symbolic_conds = get_possible_fields_symbolic o key solver pc in
        let concrete_conds = get_possible_fields_concrete o key solver pc in
        (* Two maps because set functions differ *)
        let rets =
          List.map
            (fun (concrete_key, _) ->
              let (o', pc') = create_object o key concrete_key in
              set_concrete_field o' concrete_key data;
              (o', pc') )
            concrete_conds
        in
        let rets =
          rets
          @ List.map
              (fun (symbolic_key, _) ->
                let (o', pc') = create_object o key symbolic_key in
                set_symbolic_field o' symbolic_key data;
                (o', pc') )
              symbolic_conds
        in
        let new_pc = create_not_pct (concrete_conds @ symbolic_conds) key in
        let check = Solver.check solver (new_pc @ pc) in
        if `Sat = check then
          let o' = clone o in
          let _ = Hashtbl.replace o'.symbolic_fields key data in
          (o', new_pc) :: rets
        else rets

  let get (o : t) (key : value) (solver : Solver.t) (pc : value2 list) :
    (t * value2 list * value option) list =
    match key with
    | V.Val (Val.Str key_s) -> (
      let res = Hashtbl.find_opt o.concrete_fields key_s in
      match res with
      | Some v -> [ (o, [], Some v) ]
      | None ->
        if Hashtbl.length o.symbolic_fields = 0 then [ (o, [], None) ]
        else
          let l =
            Hashtbl.fold
              (fun k d acc ->
                if is_key_possible key k solver pc then (k, d) :: acc else acc )
              o.symbolic_fields []
          in
          let obj_list =
            List.map
              (fun (k, v) ->
                let (o', pc') = create_object o key k in
                Hashtbl.remove o'.symbolic_fields k;
                Hashtbl.replace o'.concrete_fields key_s v;
                (o', pc', Some v) )
              l
          in
          (* Does not match any symbolic value, create new pct *)
          let new_pc = create_not_pct l key in
          if `Sat = Solver.check solver (new_pc @ pc) then
            let o' = clone o in
            (o', new_pc, None) :: obj_list
          else obj_list )
    | _ -> (
      let res = get_symbolic_field o key in
      match res with
      | Some v -> [ (o, [], Some v) ]
      | None ->
        let cond_list =
          Hashtbl.fold
            (fun k d acc ->
              let k' = V.Val (Val.Str k) in
              if is_key_possible key k' solver pc then (k', d) :: acc else acc )
            o.concrete_fields []
        in
        let cond_list =
          Hashtbl.fold
            (fun k d acc ->
              if is_key_possible key k solver pc then (k, d) :: acc else acc )
            o.symbolic_fields cond_list
        in
        (* Get objects for all possible symb and concrete equalities *)
        let rets =
          List.map
            (fun (k, v) ->
              let (o', pc') = create_object o key k in
              (o', pc', Some v) )
            cond_list
        in
        (* Does not match any symbolic value, create new pct *)
        let new_pc = create_not_pct cond_list key in
        let rets =
          if `Sat = Solver.check solver (new_pc @ pc) then
            let o' = clone o in
            (o', new_pc, None) :: rets
          else rets
        in
        rets )

  let delete (o : t) (key : value) (solver : Solver.t) (pc : value2 list) :
    (t * value2 list) list =
    match key with
    | V.Val (Val.Str s) ->
      if has_concrete_key o s then
        let _ = Hashtbl.remove o.concrete_fields s in
        [ (o, []) ]
      else if Hashtbl.length o.symbolic_fields = 0 then [ (o, []) ]
      else
        let lst = get_possible_fields_symbolic o key solver pc in
        (* create an object for each possible equality *)
        let rets =
          List.map
            (fun (k, _) ->
              let (o', pc') = create_object o key k in
              Hashtbl.remove o'.symbolic_fields k;
              (o', pc') )
            lst
        in
        (* update current object with the condition of not
           being equal to any of the existing fields *)
        let new_pc = create_not_pct lst key in
        if `Sat = Solver.check solver (new_pc @ pc) then
          let o' = clone o in
          (o', new_pc) :: rets
        else rets
    | _ -> (
      let res = get_symbolic_field o key in
      match res with
      | Some _v ->
        Hashtbl.remove o.symbolic_fields key;
        [ (o, []) ]
      | None ->
        let symbolic_list = get_possible_fields_symbolic o key solver pc in
        let concrete_list = get_possible_fields_concrete o key solver pc in
        (* Get objects for all possible symb equalities *)
        let rets =
          List.map
            (fun (k, _) ->
              let (o', pc') = create_object o key k in
              Hashtbl.remove o'.symbolic_fields k;
              (o', pc') )
            symbolic_list
        in
        (* Get objects for all possible concrete equalities *)
        let rets =
          List.map
            (fun (k, _) ->
              let (o', pc') = create_object o key k in
              let s =
                match k with
                | V.Val (Val.Str s) -> s
                | _ -> failwith "Invalid key value."
              in
              Hashtbl.remove o'.concrete_fields s;
              (o', pc') )
            concrete_list
          @ rets
        in
        (* Does not match any symbolic value, create new pct *)
        let new_pc = create_not_pct (symbolic_list @ concrete_list) key in
        if `Sat = Solver.check solver (new_pc @ pc) then
          let o' = clone o in
          (o', new_pc) :: rets
        else rets )

  (* let to_json (o : 'a t) (printer : 'a -> string) : string =
     let str_obj =
       Hashtbl.fold o ~init:"{ " ~f:(fun ~key:n ~data:v ac ->
           (if String.(ac <> "{ ") then ac ^ ", " else ac)
           ^ Printf.sprintf "\"%s\": %s" n (printer v))
     in
     str_obj ^ " }" *)

  let to_list (o : t) : (value * 'a) list =
    (*TODO add symb values*)
    concrete_to_list o @ (Hashtbl.to_seq o.symbolic_fields |> List.of_seq)

  let get_fields (o : t) : value list =
    let ret =
      Seq.map
        (fun f -> V.Val (Val.Str f))
        (Hashtbl.to_seq_keys o.concrete_fields)
    in
    List.of_seq ret @ List.of_seq @@ Hashtbl.to_seq_keys o.symbolic_fields
end
