type t =
  { clock : float
  ; file : Fpath.t
  ; rusage : Rusage.t
  ; status : Unix.process_status
  }

let pp_status fmt = function
  | Unix.WEXITED n -> Fmt.pf fmt "Exited %a" Fmt.int n
  | Unix.WSIGNALED n -> Fmt.pf fmt "Signaled %a" Fmt.int n
  | Unix.WSTOPPED n -> Fmt.pf fmt "Stopped %a" Fmt.int n

let pp fmt { clock; file; rusage; status } =
  Fmt.pf fmt "@[<v 2>%a@;%a in %a %a %a@]" Fpath.pp file pp_status status
    Fmt.float clock Fmt.float rusage.utime Fmt.float rusage.stime
