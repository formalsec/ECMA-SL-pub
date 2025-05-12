let debug = false

let debug k = if debug then k Format.eprintf

let ( let* ) = Result.bind

module Fmap = Fpath.Map

type diff =
  { a : Test_result.t
  ; b : Test_result.t
  }

let parse_results file =
  let* lines = Bos.OS.File.read_lines file in
  Ok
    (List.fold_left
       (fun map line ->
         match String.split_on_char ' ' (String.trim line) with
         | [ test; _; "SUCCESS"; _ ] ->
           Fmap.add (Fpath.v test) Test_result.Success map
         | [ test; _; "FAILURE"; _ ] ->
           Fmap.add (Fpath.v test) Test_result.Failure map
         | [ test; _; "ANOMALY"; _ ] ->
           Fmap.add (Fpath.v test) Test_result.Anomaly map
         | test :: _ :: "SKIPPED" :: _ ->
           Fmap.add (Fpath.v test) Test_result.Skipped map
         (* Ignore the other lines *)
         | _ -> map )
       Fmap.empty lines )

let diff a b =
  debug (fun k -> k "Files: %a %a@." Fpath.pp a Fpath.pp b);
  let* a_map = parse_results a in
  let* b_map = parse_results b in
  let diff =
    Fmap.merge
      (fun _ a b ->
        match (a, b) with
        | (Some a, Some b) -> (
          match (a, b) with
          | (Test_result.Success, Test_result.Success)
          | (Failure, Failure)
          | (Anomaly, Anomaly)
          | (Skipped, Skipped) ->
            None
          | ((Success | Failure | Anomaly | Skipped), _) -> Some { a; b } )
        | ((None | Some _), _) ->
          (* Should never happen *)
          assert false )
      a_map b_map
  in
  (* Sorry *)
  Format.printf "%a@."
    (Format.pp_print_iter
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;")
       (fun f tbl -> Fmap.iter (fun p status -> f (p, status)) tbl)
       (fun fmt (p, { a; b }) ->
         Format.fprintf fmt "%a: %a -> %a" Fpath.pp p Test_result.pp a
           Test_result.pp b ) )
    diff;
  Ok ()

let cli =
  let open Cmdliner in
  let fpath = ((fun str -> `Ok (Fpath.v str)), Fpath.pp) in
  let file0 = Arg.(required & pos 0 (some fpath) None & info []) in
  let file1 = Arg.(required & pos 1 (some fpath) None & info []) in
  let doc = "Diff tool for test262 output comparison" in
  let info = Cmd.info ~doc ~version:"%%VERSION%%" "diff" in
  Cmd.v info Term.(const diff $ file0 $ file1)

let result =
  let open Cmdliner in
  match Cmd.eval_value cli with
  | Ok (`Help | `Version) -> Cmd.Exit.ok
  | Ok (`Ok result) -> (
    match result with
    | Ok () -> Cmd.Exit.ok
    | Error (`Msg err) ->
      Format.eprintf "unexpected error: %s@." err;
      Cmd.Exit.some_error )
  | Error `Term -> Cmd.Exit.some_error
  | Error `Parse -> Cmd.Exit.cli_error
  | Error `Exn -> Cmd.Exit.internal_error

let () = exit result
