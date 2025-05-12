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
open EslBase.Syntax.Option
open EslSyntax
module V = Symbolic_value.M
module E = Smtml.Expr

module Make
    (Object :
      Object_intf.S2
        with type value = Smtml.Expr.t
         and type value2 = Smtml.Expr.t) : Memory_intf.S2 = struct
  type value = Object.value

  type value2 = Object.value2

  type object_ = Object.t

  type t =
    { parent : t option
    ; map : (Loc.t, object_) Hashtbl.t
    }

  let create () = { parent = None; map = Hashtbl.create 16 }

  let clone h = { parent = Some h; map = Hashtbl.create 16 }

  let insert { map; _ } obj =
    let loc = Loc.create () in
    Hashtbl.add map loc obj;
    E.(make @@ Val (App (`Op "loc", [ Int loc ])))

  let remove h l = Hashtbl.remove h.map l

  let set h key data = Hashtbl.replace h.map key data

  let find memory l =
    let rec aux { parent; map } l from_parent =
      match Hashtbl.find_opt map l with
      | Some o -> Some (o, from_parent)
      | None ->
        let* parent in
        aux parent l true
    in
    aux memory l false

  let get memory l =
    let+ (obj, from_parent) = find memory l in
    if not from_parent then obj
    else
      let obj = Object.clone obj in
      set memory l obj;
      obj

  let get_field heap l field solver pc =
    let obj = get heap l in
    let res = Option.bind obj (fun o -> Some (Object.get o field solver pc)) in
    match res with
    | None ->
      Log.fail "get Return is never none. loc: %a %a" Loc.pp l V.pp field
    | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc, v) ] ->
        set heap l obj;
        [ (heap, pc, v) ]
      | _ ->
        List.map
          (fun (obj, pc, v) ->
            let heap' = clone heap in
            set heap' l obj;
            (heap', pc, v) )
          objs )

  let has_field heap loc field solver pc =
    let res = get_field heap loc field solver pc in
    List.map
      (fun (new_heap, new_pc, v) ->
        let v' = E.Bool.v (Option.is_some v) in
        (new_heap, new_pc, v') )
      res

  let set_field heap loc ~field ~data solver pc =
    let obj = get heap loc in
    let res =
      Option.bind obj (fun o -> Some (Object.set o ~key:field ~data solver pc))
    in
    match res with
    | None -> Log.fail "set Return is never none. loc: %a" Loc.pp loc
    | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc) ] ->
        set heap loc obj;
        [ (heap, pc) ]
      | _ ->
        List.map
          (fun (obj, pc) ->
            let heap' = clone heap in
            set heap' loc obj;
            (heap', pc) )
          objs )

  let delete_field heap loc field solver pc =
    let obj = get heap loc in
    let res =
      Option.bind obj (fun o -> Some (Object.delete o field solver pc))
    in
    match res with
    | None -> Log.fail "delete Return is never none. loc: %a" Loc.pp loc
    | Some objs -> (
      (* Don't clone heap unless necessary *)
      match objs with
      | [ (obj, pc') ] ->
        set heap loc obj;
        [ (heap, pc') ]
      | _ ->
        List.map
          (fun (obj, pc) ->
            let heap' = clone heap in
            set heap' loc obj;
            (heap', pc) )
          objs )
end
