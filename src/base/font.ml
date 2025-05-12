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

module Config = struct
  let colored = ref true
end

let colored (fdesc : Unix.file_descr option) (ppf : Format.formatter) : bool =
  !Config.colored
  && Option.fold ~none:true ~some:Terminal.colored fdesc
  && (phys_equal ppf Fmt.stdout || phys_equal ppf Fmt.stderr)

type font_v =
  | Normal
  | Bold
  | Faint
  | Italic
  | Underline
  | Blink
  | Blinkfast
  | Negative
  | Conceal
  | Strike
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Purple
  | Cyan
  | White

type t = font_v list

let pp_font_v : font_v Fmt.t =
 fun ppf font_v ->
  match font_v with
  | Normal -> Fmt.string ppf "0"
  | Bold -> Fmt.string ppf "1"
  | Faint -> Fmt.string ppf "2"
  | Italic -> Fmt.string ppf "3"
  | Underline -> Fmt.string ppf "4"
  | Blink -> Fmt.string ppf "5"
  | Blinkfast -> Fmt.string ppf "6"
  | Negative -> Fmt.string ppf "7"
  | Conceal -> Fmt.string ppf "8"
  | Strike -> Fmt.string ppf "9"
  | Black -> Fmt.string ppf "30"
  | Red -> Fmt.string ppf "31"
  | Green -> Fmt.string ppf "32"
  | Yellow -> Fmt.string ppf "33"
  | Blue -> Fmt.string ppf "34"
  | Purple -> Fmt.string ppf "35"
  | Cyan -> Fmt.string ppf "36"
  | White -> Fmt.string ppf "37"

let pp_font : t Fmt.t =
 fun ppf ->
  Fmt.pf ppf "\027[%am"
    (Fmt.list ~sep:(fun fmt () -> Fmt.string fmt ", ") pp_font_v)

let pp ?(fdesc : Unix.file_descr option = None) (font : t) (pp_v : 'a Fmt.t)
  (ppf : Format.formatter) (v : 'a) : unit =
  if not (colored fdesc ppf) then pp_v ppf v
  else Fmt.pf ppf "%a%a%a" pp_font font pp_v v pp_font [ Normal ]

let str ?(fdesc : Unix.file_descr option = None) (font : t) (pp_v : 'a Fmt.t)
  (v : 'a) : string =
  Fmt.str "%a" (pp ~fdesc font pp_v) v

let pp_out font pp_v ppf v = pp ~fdesc:(Some Unix.stdout) font pp_v ppf v

let pp_err font pp_v ppf v = pp ~fdesc:(Some Unix.stderr) font pp_v ppf v

let str_out font pp_v v = str ~fdesc:(Some Unix.stdout) font pp_v v

let str_err font pp_v v = str ~fdesc:(Some Unix.stderr) font pp_v v

let pp_text_out font ppf s = pp ~fdesc:(Some Unix.stdout) font Fmt.string ppf s

let pp_text_err font ppf s = pp ~fdesc:(Some Unix.stderr) font Fmt.string ppf s

let str_text_out font s = str ~fdesc:(Some Unix.stdout) font Fmt.string s

let str_text_err font s = str ~fdesc:(Some Unix.stderr) font Fmt.string s
