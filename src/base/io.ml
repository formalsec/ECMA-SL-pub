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

let[@inline] fin (process_f : in_channel -> 'a) (file : string) : 'a =
  In_channel.with_open_text file process_f

let[@inline] fout (process_f : out_channel -> 'a) (file : string) : 'a =
  Out_channel.with_open_text file process_f

let[@inline] read_in_channel (ic : in_channel) : string =
  In_channel.input_all ic

let[@inline] write_out_channel (data : string) (oc : out_channel) : unit =
  Out_channel.output_string oc data

let[@inline] read_file (file : string) : string = fin read_in_channel file

let[@inline] write_file (file : string) (data : string) : unit =
  fout (write_out_channel data) file
