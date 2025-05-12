open Ecma_sl

type t =
  { flags : string list
  ; error : Value.t option
  }

let make flags error = { flags; error }

let error { error; _ } = error

let pp_error =
  let none ppf () = Fmt.string ppf "none" in
  Fmt.option ~none Value.pp

let pp fmt { flags; error } =
  Fmt.pf fmt "@[<v 1>flags:@ @[<h>[%a]@]@;error:@ %a@]"
    Fmt.(list ~sep:comma string)
    flags pp_error error

module Parser = struct
  let regex (re : string) (text : string) : string option =
    try
      ignore (Str.search_forward (Str.regexp re) text 0);
      Some (Str.matched_group 1 text)
    with Not_found -> None

  let parse_test262_flags (metadata : string) : string list =
    let flags = regex "^flags: ?\\[\\(.+\\)\\]$" metadata in
    Option.fold ~none:[] ~some:(String.split_on_char ',') flags

  let parse_test262_error (metadata : string) : Value.t option =
    let neg_f = Option.map (fun err -> Value.Str err) in
    let negative_f md = regex "^negative: ?\\(.+\\)$" md |> neg_f in
    let errtype_f md = regex "^ +type: ?\\(.+\\)$" md |> neg_f in
    match negative_f metadata with
    | Some _ as v -> v
    | None -> errtype_f metadata

  let parse (test : string) : t Result.t =
    match regex "^/\\*---\n\\(\\(.*\n\\)+\\)---\\*/" test with
    | None -> Error (`TestFmt "Invalid test format")
    | Some metadata ->
      let flags = parse_test262_flags metadata in
      let error = parse_test262_error metadata in
      Ok (make flags error)
end
