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

val default : unit -> t

val rename : t -> t

val pp_path : int -> string Fmt.t

val pp_report : int -> t Fmt.t

module Simple : sig
  type t =
    { input : Fpath.t
    ; result : Test_result.t
    ; time : float
    }

  val pp : int -> t Fmt.t
end

val simplify : t -> Simple.t
