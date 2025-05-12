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
  let langs : Enums.Lang.t list = Enums.Lang.[ Auto; JS; CESL ]

  type interp_config = Cmd_interpret.Options.config

  type t =
    { input : Fpath.t
    ; lang : Enums.Lang.t
    ; jsinterp : Enums.JSInterp.t
    ; harness : Fpath.t option
    ; interp_config : Cmd_interpret.Options.config
    }

  let set (input : Fpath.t) (lang : Enums.Lang.t) (jsinterp : Enums.JSInterp.t)
    (harness : Fpath.t option) (interp_config : interp_config) =
    { input; lang; jsinterp; harness; interp_config }
end

let build_ast code (file : Fpath.t) : Func.t Result.t =
  Result.esl_exec code @@ fun () ->
  let fpath = Fpath.to_string file in
  let p = Parsing.load_file code fpath |> Parsing.parse_func ~file:fpath in
  Log.debug "Sucessfuly loaded AST builder '%a'." Fpath.pp file;
  Ok p

let execute_partial code (entry : Interpreter.IEntry.t)
  (config : Options.interp_config) (interp : Prog.t) (ast : Fpath.t) :
  Interpreter.IResult.t Result.t =
  Result.esl_exec code @@ fun () ->
  let* build_ast = build_ast code ast in
  Hashtbl.replace (Prog.funcs interp) (Func.name' build_ast) build_ast;
  Ok (Cmd_interpret.interpret_partial code entry config interp)

let check_harness_return (result : Interpreter.IResult.t) : unit Result.t =
  match result.retval with
  | App (`Op "loc", [ Int loc ]) -> (
    let* (type_, _, _) = Interpreter.IResult.get_completion result.heap loc in
    match type_ with
    | App (`Op "symbol", [ Str "normal" ]) -> Ok ()
    | _ -> Result.error (`Execute "Harness return non-normal completion") )
  | _ ->
    let err = Fmt.str "Unable to setup harness: %a" Value.pp result.retval in
    Result.error (`Execute err)

let setup_program_harness code (interp : Prog.t) (harness : Fpath.t) :
  Value.t Heap.t Result.t =
  ignore Enums.Lang.(resolve_file_lang [ JS ] harness);
  let ast = Fpath.v (Filename.temp_file "ecmasl" "harness.cesl") in
  let entry = Interpreter.IEntry.default () in
  let config = Cmd_interpret.Options.default_config () in
  let* () = Cmd_encode.encode None harness (Some ast) in
  let* result = execute_partial code entry config interp ast in
  let* () = check_harness_return result in
  Log.debug "Sucessfuly linked JS harness '%a' to interpreter." Fpath.pp harness;
  Ok result.heap

let setup_execution code (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option)
  : (Prog.t * Value.t Heap.t option) Result.t =
  let finterp = Enums.JSInterp.interp jsinterp in
  let* interp = Cmd_compile.compile code true (Fpath.v finterp) in
  match harness with
  | None -> Ok (interp, None)
  | Some harness' ->
    let* static_heap = setup_program_harness code interp harness' in
    Ok (interp, Some static_heap)

let execute_cesl code ((interp, heap) : Prog.t * Value.t Heap.t option)
  (config : Options.interp_config) (input : Fpath.t) :
  Interpreter.IResult.t Result.t =
  let pre_initialized = Option.is_some heap in
  let main = if pre_initialized then "mainPreInitialized" else "main" in
  let entry = Interpreter.IEntry.{ main; heap } in
  let* result = execute_partial code entry config interp input in
  let retval = result.retval in
  Log.debug "Sucessfuly evaluated program with return '%a'." Value.pp retval;
  Ok result

let execute_js code (setup : Prog.t * Value.t Heap.t option)
  (config : Options.interp_config) (input : Fpath.t) :
  Interpreter.IResult.t Result.t =
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.cesl") in
  let* () = Cmd_encode.encode None input (Some ast) in
  execute_cesl code setup config ast

let run () (opts : Options.t) : unit Result.t =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let code = Code_utils.create () in
  let* setup = setup_execution code opts.jsinterp opts.harness in
  Cmd_interpret.log_metrics opts.interp_config.instrument.profiler
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some JS -> execute_js code setup opts.interp_config opts.input
  | Some CESL -> execute_cesl code setup opts.interp_config opts.input
  | _ -> execute_js code setup opts.interp_config opts.input
