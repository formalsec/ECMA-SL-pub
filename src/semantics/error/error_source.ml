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

type t = Source.at

module ErrSrcFmt (ErrorType : Error_type.ERROR_TYPE) = struct
  type location =
    { file : string
    ; line : int
    ; lpos : int
    ; rpos : int
    }

  let location (at : Source.at) : location =
    let (file, line, lpos) = (at.file, at.lpos.line, at.lpos.col) in
    let rpos = if at.lpos.line = at.rpos.line then at.rpos.col else -1 in
    { file; line; lpos; rpos }

  let format_code (line : string) : int * string =
    let start = Re.search_forward (Re.regexp "[^ \t\r\n]") line 0 in
    let line = String.sub line start (String.length line - start) in
    (start, line)

  let pp_loc : location Fmt.t =
    Font.pp_err [ Font.Italic; Font.Faint ] @@ fun ppf loc ->
    Fmt.pf ppf "File %S, line %d, characters %d-%d" loc.file loc.line loc.lpos
      loc.rpos

  let pp_indent (ppf : Format.formatter) (lineno : int) : unit =
    let lineno_sz = String.length (string_of_int lineno) in
    Fmt.string ppf (String.make (lineno_sz + 5) ' ')

  let pp_hglt (ppf : Format.formatter) ((code, lpos, rpos) : string * int * int)
    : unit =
    let pp_font = Font.pp_text_err ErrorType.font in
    let code' = Re.global_replace (Re.regexp "[^ \t\r\n]") " " code in
    Fmt.pf ppf "%s%a" (String.sub code' 0 lpos) pp_font
      (String.make (rpos - lpos) '^')

  let pp (ppf : Format.formatter) (code, at) : unit =
    if not (Source.is_none at) then
      let loc = location at in
      let (_, line) = Code_utils.(line (file code loc.file) loc.line) in
      let rpos' =
        if not (loc.rpos = -1) then loc.rpos else String.length line
      in
      let (start, code) = format_code line in
      let (lpos, rpos) = (loc.lpos - start, rpos' - start) in
      Fmt.pf ppf "@\n%a@\n%d |   %s@\n%a%a" pp_loc loc loc.line code pp_indent
        loc.line pp_hglt (code, lpos, rpos)

  let str v : string = Fmt.str "%a" pp v
end
