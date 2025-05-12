open Ecma_sl

type t =
  { section : string
  ; mutable time : float
  ; mutable success : int
  ; mutable failure : int
  ; mutable anomaly : int
  ; mutable skipped : int
  ; items : (string, item) Hashtbl.t
  }

and item =
  | Test of Test_record.Simple.t
  | Tree of t

let create (section : string) : t =
  let (time, success, failure, anomaly, skipped) = (0.0, 0, 0, 0, 0) in
  let items = Hashtbl.create !Base.default_hashtbl_sz in
  { section; time; success; failure; anomaly; skipped; items }

let total (tree : t) : int = tree.success + tree.failure + tree.anomaly

let rec add (tree : t) (record : Test_record.t) (sections : string list) :
  Test_record.t Result.t =
  match sections with
  | ([] | [ "." ]) when Hashtbl.mem tree.items record.name ->
    let record' = Test_record.rename record in
    Log.warn "duplicated test identifier: renaming '%a' to '%a'" Fpath.pp
      record.input Fpath.pp record'.input;
    add tree record' sections
  | [] | [ "." ] ->
    Hashtbl.add tree.items record.name (Test (Test_record.simplify record));
    Ok record
  | sec :: secs -> begin
    match Hashtbl.find_opt tree.items sec with
    | Some (Test _) -> Log.fail "unexpected test tree format"
    | Some (Tree tree') -> add tree' record secs
    | None ->
      let tree' = create sec in
      Hashtbl.add tree.items sec (Tree tree');
      add tree' record secs
  end

let rec count_item (item : item) : float * int * int * int * int =
  match item with
  | Test { time; result = Success; _ } -> (time, 1, 0, 0, 0)
  | Test { time; result = Failure; _ } -> (time, 0, 1, 0, 0)
  | Test { time; result = Anomaly; _ } -> (time, 0, 0, 1, 0)
  | Test { result = Skipped; _ } -> (0.0, 0, 0, 0, 1)
  | Tree tree ->
    count_results tree;
    (tree.time, tree.success, tree.failure, tree.anomaly, tree.skipped)

and count_results (tree : t) : unit =
  let count_f tree _ item =
    let (time, success, failure, anomaly, skipped) = count_item item in
    tree.time <- tree.time +. time;
    tree.success <- tree.success + success;
    tree.failure <- tree.failure + failure;
    tree.anomaly <- tree.anomaly + anomaly;
    tree.skipped <- tree.skipped + skipped
  in
  Hashtbl.iter (count_f tree) tree.items

let pp_status_header term_width (ppf : Format.formatter) () : unit =
  let line = String.make (term_width - 1) '-' in
  let header = Fmt.str "%s\n ECMA-SL Test Summary:\n" line in
  Font.pp_text_out [ Cyan ] ppf header

let pp_summary_header term_width (ppf : Format.formatter) () : unit =
  let line = String.make (term_width - 1) '-' in
  Fmt.pf ppf "%a@\n@\nTest Summary:@\n" (Font.pp_text_out [ Cyan ]) line

let rec pp_status term_width (ppf : Format.formatter) (tree : t) : unit =
  let pp_item ppf = function
    | Test record ->
      Fmt.pf ppf "%a@\n" (Test_record.Simple.pp term_width) record
    | Tree tree' -> pp_status term_width ppf tree'
  in
  Fmt.(hbox (hashtbl (fun ppf (_, i) -> pp_item ppf i)) ppf tree.items)

let rec pp_section term_width (depth : int) (ppf : Format.formatter) (tree : t)
  : unit =
  let open Fmt in
  let pp_item ppf (_, i) =
    match i with
    | Tree tree -> pp_section term_width (depth + 1) ppf tree
    | _ -> ()
  in
  let pp_curr_section ppf tree =
    if depth > 0 then
      let indent = (depth - 1) * 2 in
      let limit = term_width - 32 - indent in
      let total = total tree in
      let ratio = float_of_int tree.success *. 100.0 /. float_of_int total in
      pf ppf "%s%a [%d / %d] (%.2f%%)@\n" (String.make indent ' ')
        (Test_record.pp_path limit)
        tree.section tree.success total ratio
  in
  pf ppf "%a%a" pp_curr_section tree (hbox (hashtbl pp_item)) tree.items

let pp_total (ppf : Format.formatter) (tree : t) : unit =
  let open Fmt in
  let total = total tree in
  let ratio = float_of_int tree.success *. 100.0 /. float_of_int total in
  let (hours, mins, secs, millis) = Base.format_time tree.time in
  pf ppf "Tests Successful: %d / %d (%.2f%%) | " tree.success total ratio;
  if hours > 0 then
    pf ppf "Time elapsed: %dh %dm %ds %dms@\n" hours mins secs millis
  else pf ppf "Time elapsed: %dm %ds %dms@\n" mins secs millis;
  pf ppf "Failures: %d, Anomalies: %d, Skipped: %d" tree.failure tree.anomaly
    tree.skipped

let pp_summary term_width (ppf : Format.formatter) (tree : t) : unit =
  let pp_summary_header = pp_summary_header term_width in
  let pp_section = pp_section term_width in
  Fmt.pf ppf "@\n%a@\n%a@\n%a" pp_summary_header () (pp_section 0) tree pp_total
    tree

let pp term_width (ppf : Format.formatter) (tree : t) : unit =
  let pp_status_header = pp_status_header term_width in
  let pp_status = pp_status term_width in
  let pp_summary = pp_summary term_width in
  Fmt.pf ppf "%a@\n%a%a" pp_status_header () pp_status tree pp_summary tree
