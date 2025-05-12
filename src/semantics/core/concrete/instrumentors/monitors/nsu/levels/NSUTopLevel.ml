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

  val create : string list -> t

  val empty : unit -> t
end

module SSet = Set.Make (String)

module M = struct
  include SSet

  type t = SSet.t

  let create (sl : string list) : t =
    List.fold_left (fun ac s -> SSet.add s ac) SSet.empty sl

  let empty () : t = SSet.empty
end
