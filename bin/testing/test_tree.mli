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

val create : string -> t

val total : t -> int

val add : t -> Test_record.t -> string list -> Test_record.t Result.t

val count_results : t -> unit

val pp : int -> t Fmt.t

val pp_summary : int -> t Fmt.t

val pp_total : t Fmt.t

val pp_status_header : int -> unit Fmt.t
