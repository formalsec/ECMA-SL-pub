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
open EslSyntax
open Call_stack

type store = Value.t Store.t

type t = store Call_stack.t

module RtTraceFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  let pp_frame (ppf : Format.formatter) (frame : store frame) : unit =
    let { f; s; _ } = cursor frame in
    Fmt.pf ppf "Called from '%s' in %a" (Func.name' f) pp_loc s.at

  let pp (ppf : Format.formatter) (stack : t) : unit =
    let pp_stack = Fmt.(list ~sep:(fun fmt () -> Fmt.pf fmt "@;") pp_frame) in
    match stack with
    | [] -> Log.fail "expecting non-empty call stack"
    | frame :: stack' ->
      Fmt.pf ppf "@;@[<v 4>Raised at %a@;%a@]" pp_frame frame pp_stack stack'

  let str (stack : t) : string = Fmt.str "%a" pp stack [@@inline]
end
