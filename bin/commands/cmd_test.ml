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
  let term_width : int ref = ref (Terminal.width Unix.stdout)

  let report_width = 80

  type t =
    { inputs : Fpath.t list
    ; lang : Enums.Lang.t
    ; jsinterp : Enums.JSInterp.t
    ; harness : Fpath.t option
    ; test_type : Enums.JSTest.t
    ; report : Fpath.t option
    ; interp_profiler : Enums.InterpProfiler.t
    ; webhook_url : string option
    ; jobs : int
    }

  let set (inputs : Fpath.t list) (lang : Enums.Lang.t)
    (jsinterp : Enums.JSInterp.t) (harness : Fpath.t option)
    (test_type : Enums.JSTest.t) (report : Fpath.t option)
    (interp_profiler : Enums.InterpProfiler.t) (webhook_url : string option)
    (jobs : int) : t =
    { inputs
    ; lang
    ; jsinterp
    ; harness
    ; test_type
    ; report
    ; interp_profiler
    ; webhook_url
    ; jobs
    }
end

let get_logging_width (inputs : (Fpath.t * Fpath.t) list) : int =
  let path_len_f (_, p) = Fpath.to_string p |> String.length in
  let width = 32 + (List.map path_len_f inputs |> List.fold_left max 0) in
  min !Options.term_width width

let dump_record_report (output : Files.output) (record : Test_record.t) :
  unit Result.t =
  let pp = Test_record.pp_report Options.report_width in
  match (record.result, output) with
  | (Skipped, _) -> Ok ()
  | (_, `Generated path) -> Result.bos (Bos.OS.File.writef path "%a" pp record)
  | (_, _) -> Ok ()

let rec dump_section_smry (dir : Fpath.t) (tree : Test_tree.t) : unit Result.t =
  let open Fpath in
  let dump_smry_f _ item acc =
    match ((item : Test_tree.item), acc) with
    | (_, (Error _ as err)) -> err
    | (Test _, Ok ()) -> Ok ()
    | (Tree tree', Ok ()) -> dump_section_smry (dir / tree'.section) tree'
  in
  let* () = Hashtbl.fold dump_smry_f tree.items (Ok ()) in
  let path = (dir / "report") + Enums.Lang.str TestSummary in
  Result.bos
  @@ Bos.OS.File.writef path "%a@." (Test_tree.pp !Options.term_width) tree

let record (workspace : Fpath.t) (input : Fpath.t) (output : Files.output) :
  Test_record.t Result.t =
  let rel = Option.get (Fpath.relativize ~root:workspace input) in
  let (id, name) = (Fpath.to_string rel, Fpath.filename rel) in
  let sections = String.split_on_char '/' (Filename.dirname id) in
  let* test = Result.bos (Bos.OS.File.read input) in
  Ok { (Test_record.default ()) with input; output; name; sections; test }

let process_record code (opts : Options.t)
  (env : Prog.t * Value.t Heap.t option) (workspace : Fpath.t) (input : Fpath.t)
  (output : Files.output) : Test_record.t Result.t =
  let* record = record workspace input output in
  match Enums.Lang.resolve_file_lang ~warn:false [ JS ] input with
  | Some JS ->
    let* record' = Test_parser.parse opts.test_type record in
    Test_runner.run code env record' opts.interp_profiler
  | _ -> Ok { record with result = Skipped }

let mutex = Mutex.create ()

let run_single (opts : Options.t) (env : Prog.t * Value.t Heap.t option)
  (tree : Test_tree.t) (is_parallel : bool) (workspace : Fpath.t)
  (input : Fpath.t) (output : Files.output) : unit Result.t =
  let code = Code_utils.create () in
  let* record = process_record code opts env workspace input output in
  (* TODO: Clean this mutex up. Either TestTree is immutable and we fold and
     return a TestTreee from `process_inputs` or we make TestTree a thread-safe
     data structure *)
  Mutex.protect mutex @@ fun () ->
  let* record' = Test_tree.add tree record record.sections in
  let pp_report = Test_record.Simple.pp !Options.term_width in
  if is_parallel then Eio.traceln "%a" pp_report (Test_record.simplify record')
  else Log.stdout "%a@." pp_report (Test_record.simplify record');
  let* () = dump_record_report output record' in
  match record'.result with Success -> Ok () | _ -> Result.error `Test

let test_summary (is_parallel : bool) (output : Fpath.t option)
  (total_time : float) (tree : Test_tree.t) : unit Result.t =
  Test_tree.count_results tree;
  let pp_summary = Test_tree.pp_summary !Options.term_width in
  if is_parallel then
    Eio.traceln "%a" pp_summary { tree with time = total_time }
  else Log.stdout "%a@." pp_summary { tree with time = total_time };
  match output with
  | Some dir when Fpath.is_dir_path dir -> dump_section_smry dir tree
  | Some path ->
    Result.bos
      (Bos.OS.File.writef path "%a@." (Test_tree.pp !Options.term_width) tree)
  | _ -> Ok ()

let run () ({ inputs; report; harness; jsinterp; jobs; _ } as opts : Options.t)
  : unit Result.t =
  let total_time = Base.time () in
  let* inputs = Files.generate_input_list inputs in
  Options.term_width := get_logging_width inputs;
  Log.stdout "%a@." (Test_tree.pp_status_header !Options.term_width) ();
  let code = Code_utils.create () in
  let* env = Cmd_execute.setup_execution code jsinterp harness in
  let tree = Test_tree.create "" in
  let outext = Enums.Lang.str TestReport in
  let is_parallel = jobs > 1 in
  let run_single' = run_single opts env tree is_parallel in
  let exitcode = Files.process_inputs ~jobs ~outext run_single' inputs report in
  let total_time = Base.time () -. total_time in
  let* () = test_summary is_parallel report total_time tree in
  Option.iter
    (Test_notify.post { tree with time = total_time })
    opts.webhook_url;
  exitcode
