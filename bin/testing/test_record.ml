open Ecma_sl

type t =
  { input : Fpath.t
  ; output : Files.output
  ; name : string
  ; sections : string list
  ; test : string
  ; mutable metadata : Test_metadata.t option
  ; streams : Log.Redirect.t option
  ; retval : Value.t Result.t
  ; result : Test_result.t
  ; time : float
  ; metrics : Yojson.Basic.t
  }

let default () : t =
  { input = Fpath.v Filename.null
  ; output = `None
  ; name = ""
  ; sections = []
  ; test = ""
  ; metadata = None
  ; streams = None
  ; retval = Ok Nothing
  ; result = Test_result.Skipped
  ; time = Base.time ()
  ; metrics = `Null
  }

let rename (record : t) : t =
  let rename_dup path =
    let (path', ext) = Fpath.split_ext path in
    Fpath.(v (to_string path' ^ "_1") + ext)
  in
  let rename_out = function
    | `Generated output' -> `Generated (rename_dup output')
    | output' -> output'
  in
  let input = rename_dup record.input in
  let output = rename_out record.output in
  let name = record.name ^ "_1" in
  { record with input; output; name }

let pp_path (limit : int) (ppf : Format.formatter) (path : string) : unit =
  (* TODO: Add Ecma_sl.String to the prelude *)
  let (path', _) = String.truncate limit path in
  let len = String.length path' in
  let dots = if len < limit then String.make (limit - len) '.' else "" in
  Fmt.pf ppf "%s %s" path' dots

let pp_retval (ppf : Format.formatter) (retval : Value.t Result.t) : unit =
  match retval with Ok v -> Value.pp ppf v | Error _ -> Fmt.string ppf "-"

let pp_report report_width (ppf : Format.formatter) (record : t) : unit =
  let line = String.make report_width '-' in
  let pp_streams = Fmt.option Log.Redirect.pp_captured in
  let pp_div ppf hdr = Fmt.pf ppf "@\n%s@\n%s@\n%s@\n@\n" line hdr line in
  let sep ppf () = Fmt.string ppf "/" in
  Fmt.pf ppf "%s" record.test;
  Fmt.pf ppf "%a%a" pp_div "Test Output:" pp_streams record.streams;
  Fmt.pf ppf "%a" pp_div "Test Details:";
  Fmt.pf ppf "name: %s@\n" record.name;
  Fmt.pf ppf "sections: %a@\n" Fmt.(list ~sep string) record.sections;
  Fmt.pf ppf "metadata: %a@\n" (Fmt.option Test_metadata.pp) record.metadata;
  Fmt.pf ppf "retval: %a@\n" pp_retval record.retval;
  Fmt.pf ppf "result: %a@\n" Test_result.pp record.result;
  if not (Test_result.equal record.result Skipped) then
    Fmt.pf ppf "time: %0.2fs@\n" record.time;
  if record.metrics != `Null then
    Fmt.pf ppf "%a" Cmd_interpret.InterpreterMetrics.pp record.metrics

module Simple = struct
  type t =
    { input : Fpath.t
    ; result : Test_result.t
    ; time : float
    }

  let pp term_width (ppf : Format.formatter) (record : t) : unit =
    let limit = term_width - 20 in
    let path = Fpath.to_string record.input in
    let pp_time ppf time = Fmt.pf ppf "[%02.3fs]" time in
    Fmt.pf ppf "%a " (Font.pp_out [ Faint ] (pp_path limit)) path;
    Fmt.pf ppf "%a " Test_result.pp record.result;
    if not (Test_result.equal record.result Skipped) then
      Fmt.pf ppf "%a" (Font.pp_out [ Faint ] pp_time) record.time
end

let simplify (record : t) : Simple.t =
  let { input; result; time; _ } = record in
  { input; result; time }
