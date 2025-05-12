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
open EslSyntax

module type T = sig
  type value

  type store

  val null : value

  val string : string -> value

  val equal : value -> value -> bool

  val hash : value -> int

  val compare : value -> value -> int

  val mk_symbol : string -> value

  val mk_list : value list -> value

  val mk_tuple : value * value -> value

  val is_symbolic : value -> bool

  val func : value -> (string * value list, string) Result.t

  val pp : value Fmt.t

  val to_string : value -> string

  module Bool : sig
    val const : bool -> value

    val not_ : value -> value

    val and_ : value -> value -> value

    val or_ : value -> value -> value
  end

  module Store : sig
    type bind = string

    type t = store

    val create : (bind * value) list -> t

    val mem : t -> bind -> bool

    val add_exn : t -> bind -> value -> t

    val find : t -> bind -> value option

    val pp : t Fmt.t
  end

  val eval_expr : store -> Expr.t -> value
end
