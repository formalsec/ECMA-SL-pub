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

type file = string array

type t = (string, file) Hashtbl.t

(* This module needs to return the hashtbl otherwise it leeks GiBs of memory during the `test` command *)
let create () = Hashtbl.create !Base.default_hashtbl_sz

let load_file code (fname : string) (data : string) : unit =
  let lines = String.split_on_char '\n' data in
  Hashtbl.replace code fname (Array.of_list lines)

let file_sz (file : file) : int = Array.length file [@@inline]

let file code (fname : string) : file =
  match Hashtbl.find_opt code fname with
  | Some file -> file
  | None -> Log.fail "expecting loaded file path, but got '%s'" fname

let line (file : file) (lineno : int) : int * string =
  try (lineno, Array.get file (lineno - 1))
  with Invalid_argument _ ->
    Log.fail "expecting line between 1 and %d, but got %d" (file_sz file) lineno

let rec lines (file : file) (start : int) (nlines : int) : (int * string) list =
  if nlines = 0 then []
  else line file start :: lines file (start + 1) (nlines - 1)

let codeblock ((code, at) : t * Source.at) : string list =
  let trim_line line n =
    match (at.lpos.line, at.rpos.line) with
    | (left, right) when left = n && right = n ->
      String.substr ~left:at.lpos.col ~right:at.rpos.col line
    | (l, _) when l = n -> String.substr ~left:at.lpos.col line
    | (_, r) when r = n -> String.substr ~right:at.rpos.col line
    | _ -> line
  in
  let rec trim_lines = function
    | [] -> []
    | (n, line) :: lines' -> trim_line line n :: trim_lines lines'
  in
  let start = at.lpos.line in
  let nlines = at.rpos.line - at.lpos.line + 1 in
  trim_lines (lines (file code at.file) start nlines)

let pp (ppf : Format.formatter) v : unit =
  let sep ppf () = Fmt.pf ppf "@\n" in
  Fmt.(list ~sep string) ppf (codeblock v)

let str v : string = Fmt.str "%a" pp v
