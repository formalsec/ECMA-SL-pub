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

open Cmdliner

let set_copts (debug : Enums.DebugLvl.t) (colorless : bool) : unit =
  Ecma_sl.Font.Config.colored := not colorless;
  Ecma_sl.Log.Config.log_warns := debug >= Warn;
  Ecma_sl.Log.Config.log_debugs := debug >= Full;
  begin
    Logs.set_reporter @@ Logs_fmt.reporter ();
    match debug with
    | Enums.DebugLvl.Full -> Logs.set_level (Some Logs.Debug)
    | Warn -> Logs.set_level (Some Logs.Warning)
    | None -> ()
  end

let copts =
  let open Term in
  const set_copts $ Docs.CommonOpts.debug $ Docs.CommonOpts.colorless

let compile_opts =
  let open Term in
  const Cmd_compile.Options.set
  $ Docs.FileOpts.input
  $ Docs.FileOpts.output
  $ Docs.CompileOpts.untyped

let compile_cmd =
  let open Docs.CompileCmd in
  let info = Cmd.(info "compile" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_compile.run $ copts $ compile_opts) in
  Cmd.v info term

let interpreter_instrument =
  let open Term in
  const Cmd_interpret.Options.set_instrument
  $ Docs.InterpretOpts.tracer
  $ Docs.InterpretOpts.tracer_loc
  $ Docs.InterpretOpts.tracer_depth
  $ Docs.InterpretOpts.debugger
  $ Docs.InterpretOpts.profiler

let interpreter_config =
  let open Term in
  const Cmd_interpret.Options.set_config
  $ Docs.InterpretOpts.print_depth
  $ Docs.InterpretOpts.show_exitval
  $ interpreter_instrument

let interpret_opts =
  let open Term in
  const Cmd_interpret.Options.set
  $ Docs.FileOpts.input
  $ Docs.InterpretOpts.lang
  $ Docs.InterpretOpts.main
  $ Docs.CompileOpts.untyped
  $ interpreter_config

let interpret_cmd =
  let open Docs.InterpretCmd in
  let info = Cmd.(info "interpret" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_interpret.run $ copts $ interpret_opts) in
  Cmd.v info term

let encode_opts =
  let open Term in
  const Cmd_encode.Options.set
  $ Docs.FileOpts.inputs
  $ Docs.FileOpts.output
  $ Docs.EncodeOpts.builder

let encode_cmd =
  let open Docs.EncodeCmd in
  let info = Cmd.(info "encode" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_encode.run $ copts $ encode_opts) in
  Cmd.v info term

let execute_opts =
  let open Term in
  const Cmd_execute.Options.set
  $ Docs.FileOpts.input
  $ Docs.ExecuteOpts.lang
  $ Docs.ExecuteOpts.jsinterp
  $ Docs.ExecuteOpts.harness
  $ interpreter_config

let execute_cmd =
  let open Docs.ExecuteCmd in
  let info = Cmd.(info "execute" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_execute.run $ copts $ execute_opts) in
  Cmd.v info term

let test_opts =
  let open Term in
  const Cmd_test.Options.set
  $ Docs.FileOpts.inputs
  $ Docs.ExecuteOpts.lang
  $ Docs.ExecuteOpts.jsinterp
  $ Docs.ExecuteOpts.harness
  $ Docs.TestOpts.test_type
  $ Docs.TestOpts.report
  $ Docs.InterpretOpts.profiler
  $ Docs.TestOpts.webhook_url
  $ Docs.TestOpts.jobs

let test_cmd =
  let open Docs.TestCmd in
  let info = Cmd.(info "test" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  let term = Term.(const Cmd_test.run $ copts $ test_opts) in
  Cmd.v info term

let symbolic_term =
  let open Term.Syntax in
  let+ () = copts
  and+ input = Docs.FileOpts.input
  and+ lang = Docs.SymbolicOpts.lang
  and+ target = Docs.SymbolicOpts.target
  and+ workspace = Docs.SymbolicOpts.workspace
  and+ harness = Docs.ExecuteOpts.harness in
  Cmd_symbolic.run ~input ~lang ~target ~workspace ~harness

let symbolic_cmd =
  let open Docs.SymbolicCmd in
  let info = Cmd.(info "symbolic" ~sdocs ~doc ~man ~man_xrefs ~exits) in
  Cmd.v info symbolic_term

let test_symbolic_cmd =
  let open Term.Syntax in
  let term =
    let+ prelude = Docs.ExecuteOpts.harness
    and+ inputs = Docs.FileOpts.inputs
    and+ test_type = Docs.TestOpts.test_type
    and+ webhook_url = Docs.TestOpts.webhook_url in
    Cmd_test_symbolic.run ~prelude ~inputs ~test_type ~webhook_url
  in
  let info = Cmd.info "test-sym" in
  Cmd.v info term

let cmd_list =
  [ compile_cmd
  ; interpret_cmd
  ; encode_cmd
  ; execute_cmd
  ; test_cmd
  ; symbolic_cmd
  ; test_symbolic_cmd
  ]

let main_cmd =
  let open Docs.Application in
  let default_f _ = `Help (`Pager, None) in
  let default = Term.(ret (const default_f $ copts)) in
  let info = Cmd.info "ecma-sl" ~sdocs ~doc ~version ~man ~man_xrefs ~exits in
  Cmd.group info ~default cmd_list

let returncode =
  match Cmdliner.Cmd.eval_value main_cmd with
  | Ok (`Help | `Version) -> Docs.ExitCodes.ok
  | Ok (`Ok (Ok ())) -> Docs.ExitCodes.ok
  | Ok (`Ok (Error err)) -> begin
    match err with
    | `Compile _ -> Docs.ExitCodes.compile
    | `Runtime _ -> Docs.ExitCodes.interpret
    | `Typing -> Docs.ExitCodes.typing
    | `Encode _ -> Docs.ExitCodes.encode
    | `Execute _ -> Docs.ExitCodes.execute
    | `Test -> Docs.ExitCodes.test
    | `TestFmt _ -> Docs.ExitCodes.test
    | `Symbolic sym_err -> (
      match sym_err with
      | `Abort _ -> Docs.ExitCodes.sym_abort
      | `Assert_failure _ -> Docs.ExitCodes.sym_assert_failure
      | `Eval_failure _ -> Docs.ExitCodes.sym_eval_failure
      | `Exec_failure _ -> Docs.ExitCodes.sym_exec_failure
      | `ReadFile_failure _ -> Docs.ExitCodes.sym_readFile_failure
      | `Failure _ -> Docs.ExitCodes.sym_failure )
    | `Generic _ -> Docs.ExitCodes.generic
  end
  | Error err -> begin
    match err with
    | `Term -> Docs.ExitCodes.term
    | `Parse -> Docs.ExitCodes.client
    | `Exn -> Docs.ExitCodes.internal
  end

let () = exit returncode
