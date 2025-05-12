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

module Make () = struct
  type t = int

  let create : unit -> t =
    let (next, _) = Base.make_counter 0 1 in
    next

  let equal (loc1 : t) (loc2 : t) : bool = phys_equal loc1 loc2 [@@inline]

  let hash (loc : t) : int = loc [@@inline]

  let pp (ppf : Format.formatter) (loc : t) : unit = Fmt.pf ppf "$loc_%d" loc
  [@@inline]

  let str (loc : t) : string = Fmt.str "%a" pp loc [@@inline]

  module Tbl = struct
    (* TODO: Make this a Buffer *)
    include Hashtbl.Make (struct
      type t = int

      let equal (loc1 : t) (loc2 : t) : bool = equal loc1 loc2

      let hash (loc : t) : int = hash loc
    end)

    let pp (pp_sep : Format.formatter -> unit) (pp_v : ('a * 'b) Fmt.t)
      (ppf : Format.formatter) (loctbl : 'b t) =
      let iter f tbl = iter (fun k v -> f (k, v)) tbl in
      Fmt.iter iter ~sep:(fun fmt () -> pp_sep fmt) pp_v ppf loctbl

    let str (pp_sep : Format.formatter -> unit) (pp_v : ('a * 'b) Fmt.t) loctbl
        =
      Fmt.str "%a" (pp pp_sep pp_v) loctbl
    [@@inline]
  end
end

include Make ()
