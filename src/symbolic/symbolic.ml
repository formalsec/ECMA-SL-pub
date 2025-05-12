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

open Ecma_sl

(** This module will be called `Make` in the future because it will be a functor
*)
module P = struct
  type value = Symbolic_value.value

  type store = Symbolic_value.Store.t

  type memory = Symbolic_memory.t

  type object_ = Symbolic_object.t

  module Value = Symbolic_value
  module Choice = Choice_monad.Seq
  module Thread = Choice_monad.Thread
  module Extern_func = Extern_func.Make (Symbolic_value) (Choice)

  type extern_func = Extern_func.extern_func

  module Env = struct
    module Env = Link_env.Make (Symbolic_memory)

    type t = extern_func Env.t

    type nonrec memory = memory

    let clone env = Env.clone env [@@inline]

    let get_memory _env = Choice.with_thread Thread.mem [@@inline]

    let get_func env func_id = Env.get_func env func_id [@@inline]

    let get_extern_func env func_id = Env.get_extern_func env func_id [@@inline]

    let add_memory env mem = Env.add_memory env mem [@@inline]

    let add_func env func_id func = Env.add_func env func_id func [@@inline]

    module Build = struct
      let empty () = Env.Build.empty () [@@inline]

      let add_memory m env = Env.Build.add_memory m env [@@inline]

      let add_functions funcs env = Env.Build.add_functions funcs env [@@inline]

      let add_extern_functions funcs env =
        Env.Build.add_extern_functions funcs env
      [@@inline]
    end
  end

  type env = Env.t

  let ( let*/ ) o f =
    match o with Error e -> Fmt.failwith "%s" e | Ok o -> f o

  module Store = struct
    type bind = string

    type t = store

    let create lst = Symbolic_value.Store.create lst [@@inline]

    let mem store = Symbolic_value.Store.mem store [@@inline]

    let add_exn store key data = Symbolic_value.Store.add_exn store key data
    [@@inline]

    let find store v = Symbolic_value.Store.find store v [@@inline]
  end

  module Object = struct
    type t = object_

    type nonrec value = value

    let create () = Symbolic_object.create () [@@inline]

    let to_string o = Symbolic_object.to_string o [@@inline]

    let set o ~key ~data = Symbolic_object.set o ~key ~data [@@inline]

    let get o key =
      let vals = Symbolic_object.get o key in
      let return thread (v, pc) =
        let pc_thread = Thread.pc thread in
        let solver = Thread.solver thread in
        match pc with
        | [] -> Some (Some v, thread)
        | _ -> (
          let pc' = Smtml.Expr.Set.(union pc_thread (of_list pc)) in
          if Smtml.Expr.Set.equal pc' pc_thread then Some (Some v, thread)
          else
            match Solver.check_set solver pc' with
            | `Sat -> Some (Some v, { thread with pc = pc' })
            | `Unsat | `Unknown -> None )
      in
      match vals with
      | [] -> Choice.return None
      | [ (v, pc) ] ->
        fun thread ->
          Option.fold ~none:Cont.empty ~some:Cont.return (return thread (v, pc))
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          Cont.of_list @@ List.filter_map (return thread) vals

    let delete o v = Symbolic_object.delete o v [@@inline]

    let to_list o = Symbolic_object.to_list o [@@inline]

    let has_field o v = Symbolic_object.has_field o v [@@inline]

    let get_fields o = Symbolic_object.get_fields o [@@inline]
  end

  module Memory = struct
    type t = memory

    type nonrec object_ = object_

    type nonrec value = value

    let create () = Symbolic_memory.create () [@@inline]

    let clone m = Symbolic_memory.clone m [@@inline]

    let insert m o = Symbolic_memory.insert m o [@@inline]

    let remove m loc = Symbolic_memory.remove m loc [@@inline]

    let set m loc o = Symbolic_memory.set m loc o [@@inline]

    let get m loc = Symbolic_memory.get m loc [@@inline]

    let has_field m x v = Symbolic_memory.has_field m x v [@@inline]

    let get_field h loc v =
      let field_vals = Symbolic_memory.get_field h loc v in
      let return thread (v, pc) =
        let pc_thread = Thread.pc thread in
        let solver = Thread.solver thread in
        match pc with
        | [] -> Some (Some v, thread)
        | _ -> (
          let pc' = Smtml.Expr.Set.(union pc_thread (of_list pc)) in
          if Smtml.Expr.Set.equal pc' pc_thread then Some (Some v, thread)
          else
            match Solver.check_set solver pc' with
            | `Sat -> Some (Some v, { thread with pc = pc' })
            | `Unsat | `Unknown -> None )
      in
      match field_vals with
      | [] -> Choice.return None
      | [ (v, pc) ] -> (
        fun thread ->
          match return thread (v, pc) with
          | None -> Cont.empty
          | Some a -> Cont.return a )
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          Cont.of_list @@ List.filter_map (return thread) field_vals

    let set_field m loc ~field ~data =
      Symbolic_memory.set_field m loc ~field ~data
    [@@inline]

    let delete_field m loc v = Symbolic_memory.delete_field m loc v [@@inline]

    let to_string h = Fmt.str "%a" Symbolic_memory.pp h [@@inline]

    let loc v =
      let*/ locs = Symbolic_memory.loc v in
      let return thread (cond, v) =
        let pc = Thread.pc thread in
        let solver = Thread.solver thread in
        match cond with
        | None -> Some (v, thread)
        | Some c -> (
          let pc = Smtml.Expr.Set.add c pc in
          if Smtml.Expr.Set.equal pc (Thread.pc thread) then Some (v, thread)
          else
            match Solver.check_set solver pc with
            | `Sat -> Some (v, { thread with pc })
            | `Unsat | `Unknown -> None )
      in
      match locs with
      | [] ->
        fun _thread ->
          Logs.err (fun k -> k "   symbolic : no loc");
          Cont.empty
      | [ (c, v) ] -> (
        fun thread ->
          match return thread (c, v) with
          | None -> Cont.empty
          | Some a -> Cont.return a )
      | _ ->
        fun thread ->
          let thread = Thread.clone thread in
          Cont.of_list @@ List.filter_map (return thread) locs

    let pp ppf v = Symbolic_memory.pp ppf v [@@inline]

    let pp_val m fmt v = Symbolic_memory.pp_val m fmt v [@@inline]
  end
end

include P
