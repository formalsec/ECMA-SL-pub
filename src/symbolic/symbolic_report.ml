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

  val to_json : t -> Yojson.t
end

module type S = sig
  type t
end

module Make (Error : M) = struct
  type t =
    { filename : Fpath.t
    ; mutable execution_time : float
    ; mutable solver_time : float
    ; mutable solver_queries : int
    ; mutable num_failures : int
    ; mutable failures : Error.t list
    }

  let to_json
    { filename
    ; execution_time
    ; solver_time
    ; solver_queries
    ; num_failures
    ; failures
    } =
    `Assoc
      [ ("filename", `String (Fpath.to_string filename))
      ; ("execution_time", `Float execution_time)
      ; ("solver_time", `Float solver_time)
      ; ("solver_queries", `Int solver_queries)
      ; ("num_failures", `Int num_failures)
      ; ("failures", `List (List.map Error.to_json failures))
      ]
end
