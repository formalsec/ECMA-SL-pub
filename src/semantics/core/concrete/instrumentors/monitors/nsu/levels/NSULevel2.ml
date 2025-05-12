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

module SSet = Set.Make (String)

exception Except of string

type t =
  | High
  | MidOne
  | MidTwo
  | Low

let top () : SSet.t option ref = raise (Except "Illegal Lattice operation ")

let flows () : (SSet.t * SSet.t) list ref =
  raise (Except "Illegal Lattice operation ")

let addFlow (_ : t) (_ : t) : unit = raise (Except "Illegal Lattice operation ")

let setTop (_ : t) : unit = raise (Except "Illegal Lattice operation ")

let str (l : t) =
  match l with
  | High -> "high"
  | MidOne -> "midone"
  | MidTwo -> "midtwo"
  | Low -> "low"

let get_low () : t = Low

let get_high () : t = High

let parse_lvl (str : string) : t =
  if String.equal str "low" then Low
  else if String.equal str "high" then High
  else if String.equal str "midone" then MidOne
  else if String.equal str "midtwo" then MidTwo
  else raise (Except (Fmt.str "Unknown Level -%s" str))

let lub (l1 : t) (l2 : t) : t =
  match (l1, l2) with
  | (High, _) | (_, High) | (MidOne, MidTwo) | (MidTwo, MidOne) -> High
  | (MidOne, _) | (_, MidOne) -> MidOne
  | (MidTwo, _) | (_, MidTwo) -> MidTwo
  | (_, _) -> Low

let lubn (lst : t list) : t = List.fold_left lub Low lst

let leq (l1 : t) (l2 : t) : bool =
  match (l1, l2) with
  | (_, High) -> true
  | (Low, _) -> true
  | (MidOne, MidOne) -> true
  | (MidTwo, MidTwo) -> true
  | (_, _) -> false
