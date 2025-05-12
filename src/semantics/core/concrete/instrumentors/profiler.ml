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
open EslBase

module ExecutionTime = struct
  type t =
    { start : float
    ; stop : float
    ; diff : float
    }

  let create () : t = { start = -1.0; stop = -1.0; diff = -1.0 } [@@inline]

  let start (ts : t) : t = { ts with start = Base.time () } [@@inline]

  let diff (ts : t) : t = { ts with diff = ts.stop -. ts.start } [@@inline]

  let stop (ts : t) : t = diff @@ { ts with stop = Base.time () } [@@inline]

  let json (ts : t) : Yojson.Basic.t = `Assoc [ ("exec_time", `Float ts.diff) ]
end

module MemoryUsage = struct
  type t =
    { heap_n : int
    ; heap_sz : int
    }

  let create () : t = { heap_n = 0; heap_sz = 0 } [@@inline]

  let calculate (heap : 'a Heap.t) : t =
    let heap_n = Heap.length heap in
    let heap_sz = Obj.(reachable_words (repr heap.map)) in
    { heap_n; heap_sz }

  let json (mem : t) : Yojson.Basic.t =
    `Assoc [ ("heap_objs", `Int mem.heap_n); ("heap_size", `Int mem.heap_sz) ]
end

module ProgCounter = struct
  type t =
    { calls : int
    ; stmts : int
    ; exprs : int
    }

  type item =
    [ `Call
    | `Stmt
    | `Expr
    ]

  let create () : t = { calls = 0; stmts = 0; exprs = 0 } [@@inline]

  let count (ctr : t) (item : item) : t =
    match item with
    | `Call -> { ctr with calls = ctr.calls + 1 }
    | `Stmt -> { ctr with stmts = ctr.stmts + 1 }
    | `Expr -> { ctr with exprs = ctr.exprs + 1 }

  let json (ctr : t) : Yojson.Basic.t =
    `Assoc
      [ ("func_calls", `Int ctr.calls)
      ; ("stmt_evals", `Int ctr.stmts)
      ; ("expr_evals", `Int ctr.exprs)
      ]
end

module type M = sig
  type t

  val initial_state : unit -> t

  val start : t -> unit

  val stop : t -> 'a Heap.t -> unit

  val count : t -> ProgCounter.item -> unit

  val json : t -> Yojson.Basic.t
end

module Disable : M = struct
  type t = unit

  let initial_state () : t = ()

  let start (_ : t) : unit = ()

  let stop (_ : t) (_ : 'a Heap.t) : unit = ()

  let count (_ : t) (_ : ProgCounter.item) : unit = ()

  let json (_ : t) : Yojson.Basic.t = `Assoc []
end

module Time : M = struct
  type t = { mutable timer : ExecutionTime.t }

  let initial_state () : t = { timer = ExecutionTime.create () }

  let start (metrics : t) : unit =
    metrics.timer <- ExecutionTime.start metrics.timer

  let stop (metrics : t) (_ : 'a Heap.t) : unit =
    metrics.timer <- ExecutionTime.stop metrics.timer

  let count (_ : t) (_ : ProgCounter.item) : unit = ()

  let json (metrics : t) : Yojson.Basic.t =
    `Assoc [ ("timer", ExecutionTime.json metrics.timer) ]
end

module Full : M = struct
  type t =
    { mutable timer : ExecutionTime.t
    ; mutable memory : MemoryUsage.t
    ; mutable counter : ProgCounter.t
    }

  let initial_state () : t =
    { timer = ExecutionTime.create ()
    ; memory = MemoryUsage.create ()
    ; counter = ProgCounter.create ()
    }

  let start (metrics : t) : unit =
    metrics.timer <- ExecutionTime.start metrics.timer

  let stop (metrics : t) (heap : 'a Heap.t) : unit =
    metrics.timer <- ExecutionTime.stop metrics.timer;
    metrics.memory <- MemoryUsage.calculate heap

  let count (metrics : t) (item : ProgCounter.item) : unit =
    metrics.counter <- ProgCounter.count metrics.counter item

  let json (metrics : t) : Yojson.Basic.t =
    `Assoc
      [ ("timer", ExecutionTime.json metrics.timer)
      ; ("memory", MemoryUsage.json metrics.memory)
      ; ("counter", ProgCounter.json metrics.counter)
      ]
end
