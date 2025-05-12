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
include String

let substr ?(left : int option) ?(right : int option) (text : string) : string =
  let left' = Option.value ~default:0 left in
  let right' = Option.value ~default:(length text) right in
  sub text left' (right' - left')

let split_at_index (i : int) (text : string) : string * string =
  (substr ~right:i text, substr ~left:i text)

let rec split_with_len (len : int) (text : string) : string list =
  if length text > len then
    let (left, right) = split_at_index len text in
    left :: split_with_len len right
  else [ text ]

let truncate (limit : int) (text : string) : string * bool =
  let truncate_line line trunc =
    try if length line > limit then (sub line 0 limit, true) else (line, trunc)
    with Invalid_argument _ -> ("", true)
  in
  match split_on_char '\n' text with
  | [] -> ("", false)
  | line :: [] -> truncate_line line false
  | line :: _ -> truncate_line line true
