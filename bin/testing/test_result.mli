type t =
  | Success
  | Failure
  | Anomaly
  | Skipped

val equal : t -> t -> bool

val pp : t Fmt.t
