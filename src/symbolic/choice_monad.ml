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
module Value = Symbolic_value
module Memory = Symbolic_memory
module Optimizer = Smtml.Optimizer.Z3

module Thread = struct
  type t =
    { solver : Solver.t
    ; pc : Smtml.Expr.Set.t
    ; mem : Memory.t
    ; optimizer : Optimizer.t
    }

  let create ?(timeout = 60) () =
    (* Timeout is given in milliseconds *)
    let timeout = timeout * 1000 in
    { solver =
        Solver.create ~params:Smtml.Params.(default () $ (Timeout, timeout)) ()
    ; pc = Smtml.Expr.Set.empty
    ; mem = Memory.create ()
    ; optimizer = Optimizer.create ()
    }

  let solver t = t.solver

  let pc t = t.pc

  let mem t = t.mem

  let optimizer t = t.optimizer

  let add_pc t (v : Smtml.Expr.t) =
    match Smtml.Expr.view v with
    | Val True -> t
    | _ -> { t with pc = Smtml.Expr.Set.add v t.pc }

  let clone { solver; optimizer; pc; mem } =
    let mem = Memory.clone mem in
    { solver; optimizer; pc; mem }
end

let is_sat = function `Sat -> true | _ -> false

module Seq = struct
  type thread = Thread.t

  type 'a t = thread -> ('a * thread) Cont.t

  let return (v : 'a) : 'a t = fun t -> Cont.return (v, t)

  let stop : 'a t = fun _ -> Cont.empty

  let run (v : 'a t) (thread : thread) = v thread

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let result = run v t in
    Cont.bind (fun (r, t') -> run (f r) t') result

  let ( let* ) v f = bind v f

  let map (v : 'a t) (f : 'a -> 'b) : 'b t = bind v (fun a -> return (f a))

  let ( let+ ) v f = map v f

  let with_thread (f : thread -> 'a) : 'a t =
   fun thread ->
    let result = f thread in
    Cont.return (result, thread)

  let with_mutable_thread (f : thread -> 'a * thread) : 'a t =
   fun thread ->
    let (result, thread) = f thread in
    Cont.return (result, thread)

  let check ?(add_to_pc = false) (cond : Value.value) : bool t =
   fun t ->
    let solver = Thread.solver t in
    let pc = Thread.pc t in
    match Smtml.Expr.view cond with
    | Val True -> Cont.return (true, t)
    | Val False -> Cont.return (false, t)
    | _ -> (
      let pc = Smtml.Expr.Set.add cond pc in
      match Solver.check_set solver pc with
      | `Sat ->
        let t = if add_to_pc then Thread.add_pc t cond else t in
        Cont.return (true, t)
      | `Unsat -> Cont.return (false, t)
      | `Unknown ->
        Fmt.epr "Unknown pc: %a@." (Smtml.Expr.Set.pretty Smtml.Expr.pp) pc;
        Cont.empty )

  let branch (v : Value.value) : bool t =
   fun t ->
    let solver = Thread.solver t in
    let pc = Thread.pc t in
    match Smtml.Expr.view v with
    | Val True -> Cont.return (true, t)
    | Val False -> Cont.return (false, t)
    | _ -> (
      let with_v = Smtml.Expr.Set.add v pc in
      let with_no = Smtml.Expr.Set.add (Value.Bool.not_ v) pc in
      let sat_true =
        if Smtml.Expr.Set.equal with_v pc then true
        else is_sat @@ Solver.check_set solver with_v
      in
      let sat_false =
        if Smtml.Expr.Set.equal with_no pc then true
        else is_sat @@ Solver.check_set solver with_no
      in
      match (sat_true, sat_false) with
      | (false, false) -> Cont.empty
      | (true, false) | (false, true) -> Cont.return (sat_true, t)
      | (true, true) ->
        let t0 = Thread.clone t in
        let t1 = Thread.clone t in
        Cont.cons (true, { t0 with pc = with_v })
        @@ Cont.return (false, { t1 with pc = with_no }) )

  let select_val (v : Value.value) thread =
    match Smtml.Expr.view v with
    | Val v -> Cont.return (v, thread)
    | _ -> Fmt.failwith "Unable to select value from %a" Value.pp v

  (* FIXME: Clone state? *)
  let from_list vs : 'a t =
   fun (thread : thread) ->
    Cont.of_list
    @@ List.map
         (fun v ->
           let thread = Thread.clone thread in
           (v, thread) )
         vs
end

module List = struct
  type thread = Thread.t

  type 'a t = thread -> ('a * thread) list

  let return (v : 'a) : 'a t = fun t -> [ (v, t) ]

  let stop : 'a t = fun _ -> []

  let run (v : 'a t) (thread : thread) = v thread

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let lst = run v t in
    match lst with
    | [] -> []
    | [ (r, t') ] -> run (f r) t'
    | _ -> List.concat_map (fun (r, t') -> run (f r) t') lst

  let ( let* ) v f = bind v f

  let map (v : 'a t) (f : 'a -> 'b) : 'b t = bind v (fun a -> return (f a))

  let ( let+ ) v f = map v f

  let with_thread (f : thread -> 'a) : 'a t =
   fun thread ->
    let result = f thread in
    [ (result, thread) ]

  let with_mutable_thread (f : thread -> 'a * thread) : 'a t =
   fun thread ->
    let (result, thread) = f thread in
    [ (result, thread) ]

  let check (cond : Value.value) : bool t =
   fun t ->
    let solver = Thread.solver t in
    let pc = Thread.pc t in
    match Smtml.Expr.view cond with
    | Val True -> [ (true, t) ]
    | Val False -> [ (false, t) ]
    | _ -> (
      let pc = Smtml.Expr.Set.add cond pc in
      match Solver.check_set solver pc with
      | `Sat -> [ (true, t) ]
      | `Unsat -> [ (false, t) ]
      | `Unknown ->
        Fmt.epr "Unknown pc: %a@." (Smtml.Expr.Set.pretty Smtml.Expr.pp) pc;
        [] )

  let branch (v : Value.value) : bool t =
   fun t ->
    let solver = Thread.solver t in
    let pc = Thread.pc t in
    match Smtml.Expr.view v with
    | Val True -> [ (true, t) ]
    | Val False -> [ (false, t) ]
    | _ -> (
      let with_v = Smtml.Expr.Set.add v pc in
      let with_no = Smtml.Expr.Set.add (Value.Bool.not_ v) pc in
      let sat_true =
        if Smtml.Expr.Set.equal with_v pc then true
        else is_sat @@ Solver.check_set solver with_v
      in
      let sat_false =
        if Smtml.Expr.Set.equal with_no pc then true
        else is_sat @@ Solver.check_set solver with_no
      in
      match (sat_true, sat_false) with
      | (false, false) -> []
      | (true, false) | (false, true) -> [ (sat_true, t) ]
      | (true, true) ->
        let t0 = Thread.clone t in
        let t1 = Thread.clone t in
        [ (true, { t0 with pc = with_v }); (false, { t1 with pc = with_no }) ] )

  let select_val (v : Value.value) thread =
    match Smtml.Expr.view v with
    | Val v -> [ (v, thread) ]
    | _ -> Fmt.failwith "Unable to select value from %a" Value.pp v

  (* FIXME: Clone state? *)
  let from_list vs : 'a t =
   fun (thread : thread) -> List.map (fun v -> (v, thread)) vs
end

module P : Ecma_sl.Choice_monad_intf.Complete with module V := Value = Seq
