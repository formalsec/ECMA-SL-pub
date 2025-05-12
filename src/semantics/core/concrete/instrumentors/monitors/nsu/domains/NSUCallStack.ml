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

type 'sl sf =
  | Intermediate of ('sl list * 'sl NSUStore.t * string)
  | Toplevel

type 'sl t = 'sl sf list

let create () : 'sl t = [ Toplevel ]

let pop (cs : 'sl t) : 'sl sf * 'sl t =
  match cs with
  | [] -> raise (NSUException.Except "The Security Call Stack is empty")
  | f :: frames -> (f, frames)

let push (cs : 'sl t) (frame : 'sl sf) : 'sl t = frame :: cs
