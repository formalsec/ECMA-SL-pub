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

exception Crash of Source.at * string

exception Invalid_arg of Source.at * string

module Interpreter = Interpreter_functor.Make (Symbolic)

module Make
    (Failure : sig
      type t

      val to_json : t -> Yojson.t
    end)
    () =
struct
  module Interpreter = Interpreter ()
  module Symbolic_esl_ffi = Symbolic_esl_ffi.Make ()

  let link_env filename prog =
    let module Env = Symbolic.Env in
    let env0 = Env.Build.empty () |> Env.Build.add_functions prog in
    Env.Build.add_extern_functions (Symbolic_esl_ffi.extern_cmds env0) env0
    |> Env.Build.add_extern_functions Symbolic_esl_ffi.concrete_api
    |> Env.Build.add_extern_functions (Symbolic_esl_ffi.symbolic_api filename)

  let check_return_value ~print_return_value thread = function
    | Ok result -> (
      match Smtml.Expr.view result with
      | List [ Hc.{ node = Val False; _ }; result ] ->
        let mem = Symbolic.Thread.mem thread in
        if print_return_value then
          Logs.app (fun k ->
            k "- : %a =@[<hov> %a@]" Smtml.Ty.pp (Smtml.Expr.ty result)
              (Symbolic.Memory.pp_val mem)
              result );
        Ok ()
      | List [ { node = Val True; _ }; e ] ->
        let msg =
          Fmt.str "Failure: @[<hov>uncaught exception:@ %a@]" Smtml.Expr.pp e
        in
        Logs.err (fun k -> k "%s" msg);
        Error (`Failure msg)
      | _ ->
        let msg =
          Fmt.str "Failure: @[<hov>something went terribly wrong:@ %a@]"
            Smtml.Expr.pp result
        in
        Logs.err (fun k -> k "%s" msg);
        Error (`Failure msg) )
    | Error _ as err -> err

  module Symbolic_result = Symbolic_report.Make (Failure)

  let run ?timeout ?(lazy_values = true) ?(print_return_value = true)
    ?(no_stop_at_failure = false) ?(target = "main") ~callback_out ~callback_err
    filename prog =
    Symbolic_esl_ffi.allow_lazy_values := lazy_values;
    let start = Unix.gettimeofday () in
    let env = link_env filename prog in
    let computation = Interpreter.main env target in
    let thread = Choice_monad.Thread.create ?timeout () in
    let results = Symbolic.Choice.run computation thread in
    let report =
      { Symbolic_result.filename
      ; execution_time = start
      ; solver_time = 0.0
      ; solver_queries = 0
      ; num_failures = 0
      ; failures = []
      }
    in
    let result =
      let exception Exit in
      let exit = ref None in
      let () =
        try
          results (fun (result, thread) ->
            let () = callback_out thread result in
            let result = check_return_value ~print_return_value thread result in
            (* BAD: ignoring return value because I don't care about the result *)
            match result with
            | Ok () -> ()
            | Error witness ->
              Logs.app (fun k -> k "%a" Symbolic_error.pp witness);
              report.num_failures <- succ report.num_failures;
              let witnesses = callback_err thread witness in
              report.failures <- witnesses @ report.failures;
              if no_stop_at_failure then ()
              else begin
                exit := Some witness;
                raise Exit
              end )
        with Exit -> ()
      in
      report.execution_time <- Unix.gettimeofday () -. report.execution_time;
      report.solver_time <- !Solver.solver_time;
      report.solver_queries <- !Solver.solver_count;
      match !exit with None -> Ok () | Some err -> Error err
    in
    (result, report)
end
