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

type var = string

type 'a t = (var, 'a) Hashtbl.t

let set_bind (store : 'a t) ((x, v) : var * 'a) : unit =
  if not (Hashtbl.mem store x) then Hashtbl.replace store x v
  else Log.fail "duplicated store binding '%s'" x

let default () : 'a t = Hashtbl.create !Base.default_hashtbl_sz [@@inline]

let create (binds : (var * 'a) list) : 'a t =
  let store = default () in
  List.iter (set_bind store) binds;
  store

let length (store : 'a t) : int = Hashtbl.length store [@@inline]

let get (store : 'a t) (x : var) : 'a option = Hashtbl.find_opt store x
[@@inline]

let set (store : 'a t) (x : var) (v : 'a) : unit = Hashtbl.replace store x v
[@@inline]

let pp (pp_v : 'a Fmt.t) (ppf : Format.formatter) (store : 'a t) : unit =
  let pp_bind ppf (x, v) = Fmt.pf ppf "%s: %a" x pp_v v in
  if length store = 0 then Fmt.string ppf "{}"
  else Fmt.pf ppf "{ %a }" Fmt.(hashtbl ~sep:comma pp_bind) store

let str (pp_val : 'a Fmt.t) (store : 'a t) : string =
  Fmt.str "%a" (pp pp_val) store
[@@inline]
