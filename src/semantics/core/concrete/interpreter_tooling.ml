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
  module Tracer : Tracer.M

  module Debugger : Debugger.M

  module Profiler : Profiler.M

  module Monitor : Monitor.M

  type t =
    { db : Debugger.t
    ; pf : Profiler.t
    ; mon : Monitor.t
    ; code : Code_utils.t
    }

  val initial_state : Code_utils.t -> t

  val cleanup : t -> unit
end

module Default
    (Tr : Tracer.M)
    (Db : Debugger.M)
    (Pf : Profiler.M)
    (Mon : Monitor.M) : M = struct
  module Tracer = Tr
  module Debugger = Db
  module Profiler = Pf
  module Monitor = Mon

  type t =
    { db : Debugger.t
    ; pf : Profiler.t
    ; mon : Monitor.t
    ; code : Code_utils.t
    }

  let initial_state code : t =
    { db = Debugger.initial_state ()
    ; pf = Profiler.initial_state ()
    ; mon = Monitor.initial_state ()
    ; code
    }

  let cleanup (inst : t) : unit = Debugger.cleanup inst.db
end
