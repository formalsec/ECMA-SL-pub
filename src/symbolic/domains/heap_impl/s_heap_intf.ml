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

module type SymbolicHeap = sig
  type encoded_pct = Smtml.Expression.t

  type obj

  type t

  val create : unit -> t

  val clone : t -> t

  val insert : t -> obj -> Loc.t

  val remove : t -> Loc.t -> unit

  val set : t -> Loc.t -> obj -> unit

  val get : ?setVal:bool -> t -> Loc.t -> obj option

  val assign_obj_fields :
    t -> Expr.t -> Batch.t -> encoded_pct list -> S_store.t -> Expr.t

  val assign_obj_to_list :
    t -> Expr.t -> Batch.t -> encoded_pct list -> S_store.t -> Expr.t

  val has_field :
       t
    -> Expr.t
    -> Expr.t
    -> Batch.t
    -> encoded_pct list
    -> S_store.t
    -> (t * encoded_pct list * Expr.t) list

  val get_field :
       t
    -> Expr.t
    -> Expr.t
    -> Batch.t
    -> encoded_pct list
    -> S_store.t
    -> (t * encoded_pct list * Expr.t option) list

  val set_field :
       t
    -> Expr.t
    -> Expr.t
    -> Expr.t
    -> Batch.t
    -> encoded_pct list
    -> S_store.t
    -> (t * encoded_pct list) list

  val delete_field :
       t
    -> Expr.t
    -> Expr.t
    -> Batch.t
    -> encoded_pct list
    -> S_store.t
    -> (t * encoded_pct list) list
end
