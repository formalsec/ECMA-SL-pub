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
include Smtml.Value

let void : t = Unit

let null : t = Nothing

let undefined : t = App (`Op "symbol", [ Str "undefined" ])

let loc (l : Loc.t) : t = App (`Op "loc", [ Int l ]) [@@inline]

let symbol (s : string) : t = App (`Op "symbol", [ Str s ]) [@@inline]

let is_special_number (s : string) : bool =
  List.mem s [ "nan"; "inf"; "-inf" ]
  || String.contains s 'e'
  || String.contains s 'E'

let float_str (f : float) : string =
  let f_str = Fmt.str "%.17g" f in
  if is_special_number f_str || String.contains f_str '.' then f_str
  else Fmt.str "%s.0" f_str

let pp_custom_val (pp_v : t Fmt.t) (ppf : Format.formatter) (v : t) : unit =
  match v with
  | Unit -> ()
  | Nothing -> Fmt.string ppf "null"
  | Int i -> Fmt.int ppf i
  | Real f -> Fmt.string ppf (float_str f)
  | Str s -> Fmt.pf ppf "%S" s
  | True -> Fmt.string ppf "true"
  | False -> Fmt.string ppf "false"
  | List lst -> Fmt.(brackets (list ~sep:comma pp_v)) ppf lst
  | App (`Op "loc", [ Int loc ]) -> Loc.pp ppf loc
  | App (`Op "symbol", [ Str s ]) -> Fmt.pf ppf "'%s" s
  | App (`Op fn, fvs) -> Fmt.(pf ppf "{%S}@(%a)" fn (list ~sep:comma pp_v) fvs)
  | _ -> Log.fail "Val.pp_custom_val: unexpected value '%a'" pp v

let rec pp (ppf : Format.formatter) (v : t) : unit = pp_custom_val pp ppf v

let str (v : t) : string = Fmt.str "%a" pp v [@@inline]
