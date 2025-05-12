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

open EslSyntax

module Make (O : Object_intf.S with type value = Symbolic_value.value) = struct
  type object_ = O.t

  type t =
    { parent : t option
    ; data : object_ Loc.Tbl.t
    }

  type value = Symbolic_value.value

  let create () : t = { parent = None; data = Loc.Tbl.create 512 }

  let clone (m : t) : t = { parent = Some m; data = Loc.Tbl.create 16 }

  let insert ({ data = memory; _ } : t) (o : object_) : value =
    let loc = Loc.create () in
    Loc.Tbl.replace memory loc o;
    Smtml.Expr.(value (App (`Op "loc", [ Int loc ])))

  let remove (m : t) (l : Loc.t) : unit = Loc.Tbl.remove m.data l

  let set ({ data = memory; _ } : t) (key : Loc.t) (data : object_) : unit =
    Loc.Tbl.replace memory key data

  let find memory l =
    let open Option in
    let rec aux { parent; data } l from_parent =
      match Loc.Tbl.find_opt data l with
      | Some o -> Some (o, from_parent)
      | None ->
        let* parent in
        aux parent l true
    in
    aux memory l false

  let get memory l =
    let open Smtml_prelude.Option in
    let+ (obj, from_parent) = find memory l in
    match from_parent with
    | false -> obj
    | true ->
      let obj = O.clone obj in
      set memory l obj;
      obj

  let has_field (h : t) (loc : Loc.t) (field : value) : value =
    match get h loc with
    | None -> Symbolic_value.Bool.const false
    | Some o -> O.has_field o field

  let set_field (h : t) (loc : Loc.t) ~(field : value) ~(data : value) : unit =
    match get h loc with
    | None -> ()
    | Some o ->
      let o' = O.set o ~key:field ~data in
      set h loc o'

  let get_field (h : t) (loc : Loc.t) (field : value) :
    (value * value list) list =
    match get h loc with None -> [] | Some o -> O.get o field

  let delete_field (h : t) (loc : Loc.t) (f : value) =
    match get h loc with
    | None -> ()
    | Some o ->
      let o' = O.delete o f in
      set h loc o'

  let rec pp ppf ({ data; parent } : t) =
    let pp_v ppf (key, data) = Fmt.pf ppf "%a: %a" Loc.pp key O.pp data in
    let pp_parent ppf v =
      Fmt.option (fun ppf h -> Fmt.pf ppf "%a@ <-@ " pp h) ppf v
    in
    Fmt.pf ppf "%a{ %a }" pp_parent parent
      (Loc.Tbl.pp (fun fmt -> Fmt.string fmt ", ") pp_v)
      data

  let rec unfold_ite ~(accum : value) (e : value) : (value option * int) list =
    match Smtml.Expr.view e with
    | Val (App (`Op "loc", [ Int x ])) -> [ (Some accum, x) ]
    (* | Val (App (`Op "symbol", _)) -> [ (Some accum, ~-1) ] *)
    | Triop (_, Ite, c, a, e) -> (
      match Smtml.Expr.view a with
      | Val (App (`Op "loc", [ Int l ])) ->
        let accum' = Symbolic_value.Bool.(and_ accum (not_ c)) in
        let tl = unfold_ite ~accum:accum' e in
        (Some (Symbolic_value.Bool.and_ accum c), l) :: tl
      | _ -> assert false )
    | _ -> assert false

  let loc (e : value) : ((value option * int) list, string) Result.t =
    match Smtml.Expr.view e with
    | Val (App (`Op "symbol", [ Str "undefined" ])) ->
      (* We're in an unsat path *)
      Ok []
    | Val (App (`Op "loc", [ Int l ])) -> Ok [ (None, l) ]
    | Triop (_, Ite, c, a, v) -> (
      match Smtml.Expr.view a with
      | Val (App (`Op "loc", [ Int l ])) ->
        Ok ((Some c, l) :: unfold_ite ~accum:Smtml.Expr.(unop Ty_bool Not c) v)
      | _ ->
        Fmt.epr "Value '%a' is not a loc expression" Smtml.Expr.pp e;
        Error (Fmt.str "Value '%a' is not a loc expression" Smtml.Expr.pp e) )
    | _ ->
      Fmt.epr "Value '%a' is not a loc expression" Symbolic_value.pp e;
      Ok []

  let pp_val (h : t) fmt (e : value) : unit =
    match Smtml.Expr.view e with
    | Val (App (`Op "loc", [ Int l ])) -> (
      match get h l with None -> Loc.pp fmt l | Some o -> O.pp fmt o )
    | _ -> Symbolic_value.pp fmt e
end

include Make (Symbolic_object)
