let parse (test_type : Enums.JSTest.t) (record : Test_record.t) :
  Test_record.t Result.t =
  let open Smtml_prelude.Result in
  match test_type with
  | Simple -> Ok record
  | Test262 ->
    let* metadata = Test_metadata.Parser.parse record.test in
    record.metadata <- Some metadata;
    Ok record
  | Auto -> (
    match Test_metadata.Parser.parse record.test with
    | Ok metadata ->
      record.metadata <- Some metadata;
      Ok record
    | Error _ -> Ok record )
