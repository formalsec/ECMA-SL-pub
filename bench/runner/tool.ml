type t = Symbolic

let symbolic () = Symbolic

let p f = Fpath.to_string f

let cmd tool ~workspace ~file =
  match tool with
  | Symbolic ->
    ("ecma-sl", [| "ecma-sl"; "symbolic"; "--workspace"; p workspace; p file |])
