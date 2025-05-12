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
open Source

type t = t' Source.t

and t' = string

let default : unit -> t =
  let dflt = "" @> none in
  fun () -> dflt

let equal (id1 : t) (id2 : t) : bool = String.equal id1.it id2.it [@@inline]

let pp : t Fmt.t = fun ppf id -> Fmt.string ppf id.it [@@inline]

let str (id : t) : string = Fmt.str "%a" pp id [@@inline]
