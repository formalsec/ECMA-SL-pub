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

(* This code was based on: https://github.com/OCamlPro/owi/blob/main/src/intf/choice_intf.ml *)

module type Base = sig
  module V : Value_intf.T

  (* TODO: rename to state? *)
  type thread

  type 'a t

  val return : 'a -> 'a t

  val stop : 'a t

  val run : 'a t -> thread -> ('a * thread) EslBase.Cont.t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val with_thread : (thread -> 'a) -> 'a t
end

module type Complete = sig
  include Base

  val check : ?add_to_pc:bool -> V.value -> bool t

  val branch : V.value -> bool t

  val select_val : V.value -> Smtml.Value.t t
end
