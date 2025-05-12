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
  let required_colors = 256

  let max_width = Int.max_int

  let max_height = Int.max_int
end

module Command = struct
  let terminal_cmd (fdesc : Unix.file_descr) (cmd : string) : string option =
    if Unix.isatty fdesc then
      let ic = Unix.open_process_in cmd in
      let finally () =
        let _ = Unix.close_process_in ic in
        ()
      in
      let execute () = In_channel.input_line ic in
      Fun.protect ~finally execute
    else None

  let colored (fdesc : Unix.file_descr) : bool =
    let cmd = terminal_cmd fdesc "tput colors" in
    let colored' res =
      match int_of_string_opt res with
      | None -> false
      | Some res -> res >= Config.required_colors
    in
    Option.fold ~none:false ~some:colored' cmd

  let width (fdesc : Unix.file_descr) : int =
    let cmd = terminal_cmd fdesc "tput cols" in
    let width' res =
      match int_of_string_opt res with
      | None -> Config.max_width
      | Some width -> width
    in
    Option.fold ~none:Config.max_width ~some:width' cmd

  let height (fdesc : Unix.file_descr) : int =
    let cmd = terminal_cmd fdesc "tput lines" in
    let height' res =
      match int_of_string_opt res with
      | None -> Config.max_height
      | Some height -> height
    in
    Option.fold ~none:Config.max_height ~some:height' cmd
end

module Descriptor = struct
  type t =
    { colored : bool
    ; width : int
    ; height : int
    }

  let default () : t = { colored = false; width = -1; height = -1 }

  let create (fdesc : Unix.file_descr) : t =
    { colored = Command.colored fdesc
    ; width = Command.width fdesc
    ; height = Command.height fdesc
    }
end

let stdout : Descriptor.t ref = ref (Descriptor.create Unix.stdout)

let stderr : Descriptor.t ref = ref (Descriptor.create Unix.stderr)

let get (fdesc : Unix.file_descr) : Descriptor.t option =
  if phys_equal fdesc Unix.stdout then Some !stdout
  else if phys_equal fdesc Unix.stderr then Some !stderr
  else None

let update (fdesc : Unix.file_descr) : Descriptor.t =
  let tdesc = Descriptor.create fdesc in
  if phys_equal fdesc Unix.stdout then stdout := tdesc
  else if phys_equal fdesc Unix.stderr then stderr := tdesc;
  tdesc

let colored (fdesc : Unix.file_descr) : bool =
  let colored_f tdesc = Descriptor.(tdesc.colored) in
  Option.fold ~none:(Command.colored fdesc) ~some:colored_f (get fdesc)

let width (fdesc : Unix.file_descr) : int =
  let width_f tdesc = Descriptor.(tdesc.width) in
  Option.fold ~none:(Command.width fdesc) ~some:width_f (get fdesc)

let height (fdesc : Unix.file_descr) : int =
  let height_f tdesc = Descriptor.(tdesc.height) in
  Option.fold ~none:(Command.height fdesc) ~some:height_f (get fdesc)

let size (fdesc : Unix.file_descr) : int * int =
  let default_f () = (Command.width fdesc, Command.height fdesc) in
  let size_f tdesc = Descriptor.(tdesc.width, tdesc.height) in
  Option.fold ~none:(default_f ()) ~some:size_f (get fdesc)
