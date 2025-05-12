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

open Source

type t = t' Source.t

and t' =
  | Val of Value.t
  | Var of Id.t'
  | UnOpt of Operator.unopt * t
  | BinOpt of Operator.binopt * t * t
  | TriOpt of Operator.triopt * t * t * t
  | NOpt of Operator.nopt * t list
  | Curry of t * t list

let default : unit -> t =
  let dlft = Val Value.null @> none in
  fun () -> dlft

let isvoid (e : t) : bool = match e.it with Val Unit -> true | _ -> false

let rec pp (ppf : Format.formatter) (e : t) : unit =
  match e.it with
  | Val v -> Value.pp ppf v
  | Var x -> Fmt.string ppf x
  | UnOpt (op, e') -> Fmt.pf ppf "(%a)" (Operator.unopt_pp ~pp_v:pp) (op, e')
  | BinOpt (op, e1, e2) ->
    Fmt.pf ppf "(%a)" (Operator.binopt_pp ~pp_v:pp) (op, e1, e2)
  | TriOpt (op, e1, e2, e3) ->
    Fmt.pf ppf "(%a)" (Operator.triopt_pp ~pp_v:pp) (op, e1, e2, e3)
  | NOpt (op, es) -> Operator.nopt_pp ~pp_v:pp ppf (op, es)
  | Curry (fe, es) ->
    Fmt.pf ppf "@[<h>{%a}@(%a)@]" pp fe Fmt.(list ~sep:comma pp) es

let str (e : t) : string = Fmt.str "%a" pp e [@@inline]
