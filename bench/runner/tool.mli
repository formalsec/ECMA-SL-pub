type t

val symbolic : unit -> t

val cmd : t -> workspace:Fpath.t -> file:Fpath.t -> string * string array
