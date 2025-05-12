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
    { input : Fpath.t
    ; output : Fpath.t option
    ; untyped : bool
    }

  let set (input : Fpath.t) (output : Fpath.t option) (untyped : bool) : t =
    { input; output; untyped }
end

let type_check code (untyped : bool) (p : EProg.t) : EProg.t Result.t =
  if untyped then Ok p
  else if TChecker.type_prog code p then Ok p
  else Error `Typing

let compile_pipeline code (untyped : bool) (fname : string) (fpath : string) :
  Prog.t Result.t =
  EParsing.load_file ~file:fname code fpath
  |> EParsing.parse_eprog ~file:fname fpath
  |> Preprocessor.Imports.resolve_imports ~stdlib:Share.stdlib code
  |> Preprocessor.Macros.apply_macros
  |> type_check code untyped
  |> map Compiler.compile_prog

let load code (file : Fpath.t) : Prog.t Result.t =
  Result.esl_exec code @@ fun () ->
  let fpath = Fpath.to_string file in
  let p = Parsing.load_file code fpath |> Parsing.parse_prog ~file:fpath in
  Log.debug "Sucessfuly loaded program '%a'." Fpath.pp file;
  Ok p

let compile code (untyped : bool) (file : Fpath.t) : Prog.t Result.t =
  Result.esl_exec code @@ fun () ->
  let (fname, fpath) = (Fpath.filename file, Fpath.to_string file) in
  let* p = compile_pipeline code untyped fname fpath in
  Log.debug "Sucessfuly compiled program '%a'." Fpath.pp file;
  Ok p

let run () (opts : Options.t) : unit Result.t =
  ignore Enums.Lang.(resolve_file_lang [ ESL ] opts.input);
  let code = Code_utils.create () in
  let* p = compile code opts.untyped opts.input in
  match opts.output with
  | None -> Ok (Log.stdout "%a@." Prog.pp p)
  | Some output' -> Result.bos (Bos.OS.File.writef output' "%a" Prog.pp p)
