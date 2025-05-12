include Rusage

let sub a b =
  { utime = a.utime -. b.utime
  ; stime = a.stime -. b.stime
  ; maxrss = Int64.sub a.maxrss b.maxrss
  ; ixrss = Int64.sub a.ixrss b.ixrss
  ; idrss = Int64.sub a.idrss b.idrss
  ; isrss = Int64.sub a.isrss b.isrss
  ; minflt = Int64.sub a.minflt b.minflt
  ; majflt = Int64.sub a.majflt b.majflt
  ; nswap = Int64.sub a.nswap b.nswap
  ; inblock = Int64.sub a.inblock b.inblock
  ; oublock = Int64.sub a.oublock b.oublock
  ; msgsnd = Int64.sub a.msgsnd b.msgsnd
  ; msgrcv = Int64.sub a.msgrcv b.msgrcv
  ; nsignals = Int64.sub a.nsignals b.nsignals
  ; nvcsw = Int64.sub a.nvcsw b.nvcsw
  ; nivcsw = Int64.sub a.nivcsw b.nivcsw
  }
