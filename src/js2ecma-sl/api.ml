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

(* This module is temporary and should be removed as soon as the JS2ECMA-SL is rewritten in OCaml using the Flow parser *)
open Bos

let set_output (output : string option) (cmd : Cmd.t) : Cmd.t =
  match output with
  | None -> cmd
  | Some output' -> Cmd.(add_args cmd (v "-o" % output'))

let set_builder (builder : string option) (cmd : Cmd.t) : Cmd.t =
  match builder with
  | None -> cmd
  | Some builder' -> Cmd.(add_args cmd (v "-b" % builder'))

let cmd (input : string) (output : string option) (builder : string option) :
  Cmd.t =
  Cmd.(v "js2ecma-sl" % "-s" % "-c" % "-i" % input)
  |> set_output output
  |> set_builder builder
