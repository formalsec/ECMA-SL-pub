open Ecma_sl
open Smtml_prelude.Result

let flags (record : Test_record.t) =
  match record.metadata with None -> [] | Some m -> m.flags

let error (record : Test_record.t) =
  Option.bind record.metadata Test_metadata.error

let test_skipped (record : Test_record.t) : bool =
  let skipped_f skipped = function "skip" -> true | _ -> skipped in
  List.fold_left skipped_f false (flags record)

let interp_config (profiler : Enums.InterpProfiler.t) :
  Cmd_interpret.Options.config =
  let interp_config = Cmd_interpret.Options.default_config () in
  let instrument = { interp_config.instrument with profiler } in
  { interp_config with instrument }

let set_test_flags (record : Test_record.t) : Fpath.t Result.t =
  let flags_f (test, updated) = function
    | "onlyStrict" -> ("\"use strict\";\n" ^ test, true)
    | _ -> (test, updated)
  in
  let start = (record.test, false) in
  let (test, updated) = List.fold_left flags_f start (flags record) in
  if not updated then Ok record.input
  else
    let input = Fpath.v (Filename.temp_file "ecmasl" "flagged-input.js") in
    let* () = Result.bos (Bos.OS.File.writef input "%s" test) in
    Ok input

let unfold_result (result : Interpreter.IResult.t Result.t) :
  Value.t Result.t * Value.t Heap.t option * Yojson.Basic.t =
  match result with
  | Error _ as err -> (err, None, `Null)
  | Ok result -> (Ok result.retval, Some result.heap, result.metrics)

let check_result (error : Value.t option) heap (retval : Value.t Result.t) :
  Test_result.t =
  match (retval, heap) with
  | (Ok (App (`Op "loc", [ Int loc ])), Some heap) -> (
    match (Interpreter.IResult.get_completion heap loc, error) with
    | (Ok (App (`Op "symbol", [ Str "normal" ]), _, _), None) -> Success
    | (Ok (App (`Op "symbol", [ Str "throw" ]), e1, _), Some e2) ->
      if Value.equal e1 e2 then Success else Failure
    | (Ok (_, _, _), _) -> Failure
    | _ -> Anomaly )
  | _ -> Anomaly

let execute code (env : Prog.t * Value.t Heap.t option)
  (interp_config : Cmd_interpret.Options.config) (input : Fpath.t) :
  Interpreter.IResult.t Result.t =
  try Cmd_execute.execute_js code env interp_config input
  with exn -> Result.error (`Generic (Printexc.to_string exn))

let skip_test (record : Test_record.t) : Test_record.t Result.t =
  Ok { record with result = Skipped }

let execute_test code (env : Prog.t * Value.t Heap.t option)
  (record : Test_record.t) (interp_profiler : Enums.InterpProfiler.t) :
  Test_record.t Result.t =
  let interp_config = interp_config interp_profiler in
  let* input = set_test_flags record in
  let streams = Log.Redirect.capture Shared in
  let interp_result = execute code env interp_config input in
  Log.Redirect.restore streams;
  let streams = Some streams in
  let (retval, heap, metrics) = unfold_result interp_result in
  let result = check_result (error record) heap retval in
  let time = Base.time () -. record.time in
  Ok { record with streams; retval; result; time; metrics }

let run code (env : Prog.t * Value.t Heap.t option) (record : Test_record.t)
  (interp_profiler : Enums.InterpProfiler.t) : Test_record.t Result.t =
  Log.debug "Starting test '%a'." Fpath.pp record.input;
  if test_skipped record then skip_test record
  else execute_test code env record interp_profiler
