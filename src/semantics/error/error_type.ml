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

module type ERROR_TYPE = sig
  type t

  val header : string

  val font : Font.t

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val str : t -> string
end

module ErrorTypeFmt (ErrorType : ERROR_TYPE) = struct
  let pp_cause (ppf : Format.formatter) (msg : ErrorType.t) : unit =
    let pp_font = Font.pp_text_err (Faint :: ErrorType.font) in
    Fmt.pf ppf "\n%a %a" pp_font "Caused by:" ErrorType.pp msg

  let pp_msgs (ppf : Format.formatter) (msgs : ErrorType.t list) : unit =
    match msgs with
    | [] -> Log.fail "expecting non-empty error message list"
    | main :: causes ->
      Fmt.pf ppf "%a%a" ErrorType.pp main Fmt.(list pp_cause) causes

  let pp (ppf : Format.formatter) (msgs : ErrorType.t list) : unit =
    let header = Fmt.str "%s:" ErrorType.header in
    let font = ErrorType.font in
    Fmt.pf ppf "%a %a" (Font.pp_text_err font) header pp_msgs msgs

  let str (msgs : ErrorType.t list) : string = Fmt.str "%a" pp msgs [@@inline]
end
