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

(** Best effort to try and get commit hash of HEAD *)
let get_head ?(length = 6) () =
  let open Bos in
  let short = Format.asprintf "--short=%d" length in
  let cmd = Cmd.(v "git" % "rev-parse" % short % "HEAD") in
  let output = OS.Cmd.run_out ~err:OS.Cmd.err_run_out cmd in
  match OS.Cmd.out_string ~trim:true output with
  | Ok (stdout, (_, `Exited 0)) -> stdout
  | Error (`Msg err) ->
    Format.eprintf "ERROR: %s@." err;
    "unknown"
  | Ok (stdout, (_, (`Exited _ | `Signaled _))) ->
    Format.eprintf "%s@\nWARN: Unable to fetch git HEAD@." stdout;
    "unknown"
