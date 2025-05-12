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

open Smtml_prelude.Result
module Choice = Ecma_sl_symbolic.Choice_monad.Seq
module Thread = Ecma_sl_symbolic.Choice_monad.Thread
module Solver = Ecma_sl_symbolic.Solver
module Symbolic_interpreter =
  Ecma_sl_symbolic.Symbolic_interpreter.Make
    (Ecma_sl_symbolic.Symbolic_error)
    ()

let valid_languages : Enums.Lang.t list = Enums.Lang.[ Auto; JS; ESL; CESL ]

let setup_prelude ?(strict = false) filename prelude =
  let open Bos in
  match prelude with
  | None -> Ok filename
  | Some prelude ->
    let* data_prelude = OS.File.read prelude in
    let* data_filename = OS.File.read filename in
    let filename' = Fpath.v (Filename.temp_file "ecmasl" ".js") in
    let* () =
      if strict then
        OS.File.writef filename' "\"use strict\";\n\n%s\n%s" data_prelude
          data_filename
      else OS.File.writef filename' "%s\n%s" data_prelude data_filename
    in
    Ok filename'

let prog_of_js code fpath =
  let open Ecma_sl in
  let interp = Share.es6_sym_interp () in
  let prog = Parsing.parse_prog interp in
  (* We can use interpreter will all the metadata once the value_translator can handle nary operators *)
  (* let instrument = Cmd_interpret.Options.default_instrument () in *)
  (* let* (interp, _) = Cmd_execute.setup_execution ECMARef6Sym None instrument in *)
  let ast = Fpath.v (Filename.temp_file "ecmasl" "ast.cesl") in
  let* () = Cmd_encode.encode None fpath (Some ast) in
  let+ build_ast = Cmd_execute.build_ast code ast in
  Hashtbl.replace (Prog.funcs prog) (Func.name' build_ast) build_ast;
  prog

let dispatch_prog code lang fpath =
  let valid_langs = Enums.Lang.valid_langs valid_languages lang in
  match Enums.Lang.resolve_file_lang valid_langs fpath with
  | Some CESL -> Cmd_compile.load code fpath
  | Some ESL -> Cmd_compile.compile code true fpath
  | Some JS -> prog_of_js code fpath
  | _ ->
    let msg = Fmt.str "%a :unreconized file type" Fpath.pp fpath in
    Result.error (`Generic msg)

let serialize_thread workspace =
  let open Ecma_sl in
  let mode = 0o666 in
  let (next_int, _) = Base.make_counter 0 1 in
  fun thread witness ->
    let open Fpath in
    let pc = Thread.pc thread in
    Logs.app (fun k -> k "@[<hov 1>Path condition:@;%a@]" Solver.pp_set pc);
    let solver = Thread.solver thread in
    match Solver.check_set solver pc with
    | `Unsat | `Unknown ->
      (* Should not happen. But it does? *)
      [ witness ]
    | `Sat ->
      let model = Solver.model solver in
      let path = Fmt.kstr (add_seg workspace) "witness-%d" (next_int ()) in
      let pp = Fmt.option Smtml.Model.pp in
      Logs.app (fun k -> k "@[<v 1>Model:@;%a@]" pp model);
      let pc = Smtml.Expr.Set.to_list @@ pc in
      let _ = Bos.OS.File.writef ~mode (path + ".sexp") "%a@." pp model in
      let _ =
        Bos.OS.File.writef ~mode (path + ".smtml") "%a@." Smtml.Expr.pp_smt pc
      in
      [ witness ]

let write_report workspace symbolic_report =
  let mode = 0o666 in
  let json = Symbolic_interpreter.Symbolic_result.to_json symbolic_report in
  let path = Fpath.(workspace / "symbolic-execution.json") in
  Result.bos
  @@ Bos.OS.File.writef ~mode path "%a@." (Yojson.pretty_print ~std:true) json

let run ~input ~lang ~target ~workspace ~harness =
  let* input = Result.bos @@ setup_prelude input harness in
  let code = Ecma_sl.Code_utils.create () in
  let* prog = dispatch_prog code lang input in
  let testsuite = Fpath.(workspace / "test-suite") in
  let* _ = Result.bos (Bos.OS.Dir.create ~mode:0o777 testsuite) in
  let (result, report) =
    Symbolic_interpreter.run ~print_return_value:true ~no_stop_at_failure:false
      ~target
      ~callback_out:(fun _ _ -> ())
      ~callback_err:(serialize_thread testsuite)
      input prog
  in
  Logs.debug (fun k -> k " clock: %fs" report.execution_time);
  Logs.debug (fun k -> k " query: %fs" report.solver_time);
  if report.num_failures = 0 then Logs.app (fun k -> k "All Ok!")
  else Logs.app (fun k -> k "Found %d problems!" report.num_failures);
  let* () = write_report workspace report in
  match result with Ok () -> Ok () | Error err -> Error (`Symbolic err)
