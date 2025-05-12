type t =
  | Success
  | Failure
  | Anomaly
  | Skipped

let pp fmt = function
  | Success -> Format.pp_print_string fmt "SUCCESS"
  | Failure -> Format.pp_print_string fmt "FAILURE"
  | Anomaly -> Format.pp_print_string fmt "ANOMALY"
  | Skipped -> Format.pp_print_string fmt "SKIPPED"
