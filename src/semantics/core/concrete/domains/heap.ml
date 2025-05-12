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
open EslBase
open EslSyntax

type 'a obj = 'a Object.t

type 'a t =
  { parent : 'a t option
  ; map : 'a obj Loc.Tbl.t
  }

let default () : 'a t =
  { parent = None; map = Loc.Tbl.create !Base.default_hashtbl_sz }
[@@inline]

let create () : 'a t = default () [@@inline]

let length (heap : 'a t) : int = Loc.Tbl.length heap.map [@@inline]

let extend (heap : 'a t) : 'a t = { (create ()) with parent = Some heap }
[@@inline]

let shrunk (heap : 'a t) : 'a t = Option.value ~default:heap heap.parent
[@@inline]

let rec get (heap : 'a t) (loc : Loc.t) : 'a obj option =
  (* Hack: required to call the get function recursively *)
  let get' = get in
  match Loc.Tbl.find_opt heap.map loc with
  | Some _ as obj -> obj
  | None ->
    let open Smtml_prelude.Option in
    let* parent = heap.parent in
    let+ obj = get' parent loc in
    let obj' = Object.clone obj in
    Loc.Tbl.replace heap.map loc obj';
    obj'

let set (heap : 'a t) (loc : Loc.t) (obj : 'a obj) : unit =
  Loc.Tbl.replace heap.map loc obj
[@@inline]

let pp_map (pp_v : 'a obj Fmt.t) (ppf : Format.formatter)
  (map : 'a obj Loc.Tbl.t) : unit =
  let pp_bind ppf (loc, obj) = Fmt.pf ppf "%a: %a" Loc.pp loc pp_v obj in
  if Loc.Tbl.length map = 0 then Fmt.string ppf "{}"
  else
    Fmt.pf ppf "{ %a }"
      (Loc.Tbl.pp (fun ppf -> Fmt.string ppf ", ") pp_bind)
      map

let rec pp (pp_v : 'a obj Fmt.t) (ppf : Format.formatter) (heap : 'a t) : unit =
  let pp_parent ppf heap = Fmt.pf ppf "%a <- " (pp pp_v) heap in
  Fmt.pf ppf "%a%a" (Fmt.option pp_parent) heap.parent (pp_map pp_v) heap.map

let str (pp_v : 'a obj Fmt.t) (heap : 'a t) : string =
  Fmt.str "%a" (pp pp_v) heap
[@@inline]
