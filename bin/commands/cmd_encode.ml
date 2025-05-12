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

open Ecma_sl
open Smtml_prelude.Result

module Options = struct
  type t =
    { inputs : Fpath.t list
    ; output : Fpath.t option
    ; builder : string option
    }

  let set (inputs : Fpath.t list) (output : Fpath.t option)
    (builder : string option) : t =
    { inputs; output; builder }
end

let encode (builder : string option) (input : Fpath.t) (output : Fpath.t option)
  : unit Result.t =
  let input' = Fpath.to_string input in
  let output' = Option.map Fpath.to_string output in
  match Bos.OS.Cmd.run (EslJSParser.Api.cmd input' output' builder) with
  | Ok () -> Ok (Log.debug "Sucessfuly encoded file '%a'." Fpath.pp input)
  | Error (`Msg msg) -> Result.error (`Encode msg)

let run_single (builder : string option) (_ : Fpath.t) (input : Fpath.t)
  (output : Files.output) : unit Result.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] input);
  encode builder input (Files.get_output output)

let run () (opts : Options.t) : unit Result.t =
  let* inputs = Files.generate_input_list opts.inputs in
  Files.process_inputs ~outext:(Enums.Lang.str CESL) (run_single opts.builder)
    inputs opts.output
