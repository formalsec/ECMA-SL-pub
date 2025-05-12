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
  let langs : Enums.Lang.t list = Enums.Lang.[ Auto; ESL; CESL; CESLUnattached ]

  type tracer =
    { mode : Enums.InterpTracer.t
    ; loc : bool
    ; depth : int
    }

  type instrument =
    { tracer : tracer
    ; debugger : bool
    ; profiler : Enums.InterpProfiler.t
    }

  let set_instrument (tracer_mode : Enums.InterpTracer.t) (tracer_loc : bool)
    (tracer_depth : int) (debugger : bool) (profiler : Enums.InterpProfiler.t) :
    instrument =
    let (mode, loc, depth) = (tracer_mode, tracer_loc, tracer_depth) in
    { tracer = { mode; loc; depth }; debugger; profiler }

  type config =
    { print_depth : int option
    ; resolve_exitval : bool
    ; show_exitval : bool
    ; instrument : instrument
    }

  let default_config () : config =
    let tracer = { mode = None; loc = false; depth = 0 } in
    { print_depth = None
    ; resolve_exitval = true
    ; show_exitval = false
    ; instrument = { tracer; debugger = false; profiler = None }
    }

  let set_config (print_depth : int option) (show_exitval : bool)
    (instrument : instrument) : config =
    let resolve_exitval = true in
    { print_depth; resolve_exitval; show_exitval; instrument }

  type t =
    { input : Fpath.t
    ; lang : Enums.Lang.t
    ; main : string
    ; untyped : bool
    ; config : config
    }

  let set (input : Fpath.t) (lang : Enums.Lang.t) (main : string)
    (untyped : bool) (config : config) : t =
    { input; lang; main; untyped; config }
end

module InterpreterMetrics = struct
  open Yojson.Basic
  open Yojson.Basic.Util

  let pp_el (pp : t Fmt.t) (ppf : Format.formatter) (member : t) : unit =
    if member != `Null then pp ppf member

  let pp_timer (ppf : Format.formatter) (timer : t) : unit =
    let exec_time = member "exec_time" timer |> to_float in
    let (_, _, secs, millis) = Base.format_time exec_time in
    Fmt.pf ppf "@\nexec time:  %ds%.3dms" secs millis

  let pp_memory (ppf : Format.formatter) (memory : t) : unit =
    let heap_n = member "heap_objs" memory |> to_int in
    let heap_sz = member "heap_size" memory |> to_int in
    let heap_sz_bytes = heap_sz * Sys.word_size in
    let (heap_sz_fmt, heap_sz_unit) = Base.format_bytes heap_sz_bytes in
    Fmt.pf ppf "@\nobj allocs: %d@\nheap size:  %d bytes (~%0.2f %s)" heap_n
      heap_sz_bytes heap_sz_fmt heap_sz_unit

  let pp_counter (ppf : Format.formatter) (counter : t) : unit =
    let divider = "----------" in
    let calls = member "func_calls" counter |> to_int in
    let stmts = member "stmt_evals" counter |> to_int in
    let exprs = member "expr_evals" counter |> to_int in
    Fmt.pf ppf "@\n%s@\nfunc calls: %d@\nstmt evals: %d@\nexpr evals: %d"
      divider calls stmts exprs

  let pp (ppf : Format.formatter) (metrics : t) : unit =
    let timer = Util.member "timer" metrics in
    let memory = Util.member "memory" metrics in
    let counter = Util.member "counter" metrics in
    Fmt.pf ppf "%a%a%a" (pp_el pp_timer) timer (pp_el pp_memory) memory
      (pp_el pp_counter) counter

  let log (profiler : Enums.InterpProfiler.t) (metrics : t) : unit =
    if Enums.InterpProfiler.equal profiler None then ()
    else Log.esl "Execution metrics:%a" pp metrics
end

module InterpreterInstrument = struct
  let tracer ({ mode; loc; depth } : Options.tracer) : (module Tracer.M) =
    Tracer.Config.trace_loc := loc;
    Tracer.Config.trace_depth := if depth > 0 then Some depth else None;
    match mode with
    | None -> (module Tracer.Disable : Tracer.M)
    | Call -> (module Tracer.Call : Tracer.M)
    | Step -> (module Tracer.Step : Tracer.M)
    | Full -> (module Tracer.Full : Tracer.M)
    | Core -> (module Tracer.Core : Tracer.M)

  let debugger (debugger : bool) : (module Debugger.M) =
    match debugger with
    | true -> (module Debugger.Enable : Debugger.M)
    | false -> (module Debugger.Disable : Debugger.M)

  let profiler (profiler : Enums.InterpProfiler.t) : (module Profiler.M) =
    match profiler with
    | None -> (module Profiler.Disable : Profiler.M)
    | Time -> (module Profiler.Time : Profiler.M)
    | Full -> (module Profiler.Full : Profiler.M)

  let monitor () : (module Monitor.M) = (module Monitor.Default : Monitor.M)

  let intrument (instrument : Options.instrument) :
    (module Interpreter_tooling.M) =
    let module Tracer = (val tracer instrument.tracer) in
    let module Debugger = (val debugger instrument.debugger) in
    let module Profiler = (val profiler instrument.profiler) in
    let module Monitor = (val monitor ()) in
    (module Interpreter_tooling.Default (Tracer) (Debugger) (Profiler) (Monitor))
end

let interpret_partial code (entry : Interpreter.IEntry.t)
  (config : Options.config) (prog : Prog.t) : Interpreter.IResult.t =
  let instrument = config.instrument in
  let module Instrument = (val InterpreterInstrument.intrument instrument) in
  let module ConcreteInterpreter = Interpreter.M (Instrument) in
  Interpreter.IConfig.print_depth := config.print_depth;
  Interpreter.IConfig.resolve_exitval := config.resolve_exitval;
  Interpreter.IConfig.show_exitval := config.show_exitval;
  ConcreteInterpreter.eval_prog code entry prog

let interpret code (entry : Interpreter.IEntry.t) (config : Options.config)
  (prog : Prog.t) : Interpreter.IResult.t Result.t =
  Result.esl_exec code @@ fun () ->
  let result = interpret_partial code entry config prog in
  let retval = result.retval in
  Log.debug "Sucessfuly evaluated program with return '%a'." Value.pp retval;
  Ok result

let interpret_cesl code (entry : Interpreter.IEntry.t) (config : Options.config)
  (file : Fpath.t) : Interpreter.IResult.t Result.t =
  let* p = Cmd_compile.load code file in
  interpret code entry config p

let interpret_esl code (entry : Interpreter.IEntry.t) (config : Options.config)
  (untyped : bool) (file : Fpath.t) : Interpreter.IResult.t Result.t =
  let* p = Cmd_compile.compile code untyped file in
  interpret code entry config p

let log_metrics (profiler : Enums.InterpProfiler.t)
  (result : Interpreter.IResult.t Result.t) : unit Result.t =
  match result with
  | Ok result' -> Ok (InterpreterMetrics.log profiler result'.metrics)
  | Error _ as err -> err

let run () (opts : Options.t) : unit Result.t =
  let valid_langs = Enums.Lang.valid_langs Options.langs opts.lang in
  let entry = { (Interpreter.IEntry.default ()) with main = opts.main } in
  let code = Code_utils.create () in
  log_metrics opts.config.instrument.profiler
  @@
  match Enums.Lang.resolve_file_lang valid_langs opts.input with
  | Some ESL -> interpret_esl code entry opts.config opts.untyped opts.input
  | Some CESL -> interpret_cesl code entry opts.config opts.input
  | Some CESLUnattached | _ ->
    let config = { opts.config with resolve_exitval = false } in
    interpret_cesl code entry config opts.input
