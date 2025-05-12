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

(* TODO: merge these type signatures *)
module type S = sig
  type t

  type value

  type object_

  val create : unit -> t

  val clone : t -> t

  val insert : t -> object_ -> value

  val remove : t -> Loc.t -> unit

  val set : t -> Loc.t -> object_ -> unit

  val get : t -> Loc.t -> object_ option

  val set_field : t -> Loc.t -> field:value -> data:value -> unit

  val get_field : t -> Loc.t -> value -> (value * value list) list

  val has_field : t -> Loc.t -> value -> value

  val delete_field : t -> Loc.t -> value -> unit

  val pp : t Fmt.t

  val loc : value -> ((value option * Loc.t) list, string) Result.t

  val pp_val : t -> Format.formatter -> value -> unit
end

module type S2 = sig
  type t

  type value

  type value2

  type object_

  val create : unit -> t

  val clone : t -> t

  val insert : t -> object_ -> value

  val remove : t -> Loc.t -> unit

  val set : t -> Loc.t -> object_ -> unit

  val get : t -> Loc.t -> object_ option

  val set_field :
       t
    -> Loc.t
    -> field:value
    -> data:value
    -> Solver.t
    -> value2 list
    -> (t * value2 list) list

  val get_field :
       t
    -> Loc.t
    -> value
    -> Solver.t
    -> value2 list
    -> (t * value2 list * value option) list

  val has_field :
       t
    -> Loc.t
    -> value
    -> Solver.t
    -> value2 list
    -> (t * value2 list * value) list

  val delete_field :
    t -> Loc.t -> value -> Solver.t -> value2 list -> (t * value2 list) list
end
