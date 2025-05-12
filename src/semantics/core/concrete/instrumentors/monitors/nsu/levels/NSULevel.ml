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

module type M = sig
  type t

  exception Except of string

  val top : t option ref

  val flows : (t * t) list ref

  val setTop : t -> unit

  val addFlow : t -> t -> unit

  val str : t -> string

  val parse_lvl : string -> t

  val lub : t -> t -> t

  val lubn : t list -> t

  val leq : t -> t -> bool

  val get_low : unit -> t

  val get_high : unit -> t
end

exception Except of string

type t =
  | High
  | Low

type flow = t * t

let top : t option ref = ref None

let flows : flow list ref = ref []

let all_levels : t list ref = ref []

let addFlow (_lst1 : t) (_lst2 : t) : unit =
  raise (Except "Illegal Lattice operation - addflow")

let setTop (_ : t) : unit = raise (Except "Illegal Lattice operation - setTop")

let str (l : t) = match l with High -> "high" | Low -> "low"

let get_low () : t = Low

let get_high () : t = High

let parse_lvl (str : string) : t =
  if String.equal str "low" then Low
  else if String.equal str "high" then High
  else raise (Except (Fmt.str "Unknown Level -%s" str))

let lub (l1 : t) (l2 : t) : t =
  match (l1, l2) with (High, _) | (_, High) -> High | (_, _) -> Low

let lubn (lst : t list) : t = List.fold_left lub Low lst

let leq (l1 : t) (l2 : t) : bool =
  match (l1, l2) with (_, High) -> true | (Low, _) -> true | (_, _) -> false
