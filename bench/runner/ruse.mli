type t = Rusage.t

type who = Rusage.who

val get : who -> t

val sub : t -> t -> t
