(* Copyright (C) 2022-2025 formalsec programmers
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

open Source

module PatVal = struct
  type t = t' Source.t

  and t' =
    | Var of Id.t'
    | Val of Value.t
    | None

  let pp (ppf : Format.formatter) (pv : t) : unit =
    match pv.it with
    | Var x -> Fmt.string ppf x
    | Val v -> Value.pp ppf v
    | None -> Fmt.string ppf "None"

  let str (pv : t) : string = Fmt.str "%a" pp pv [@@inline]
end

type t = t' Source.t

and t' =
  | ObjPat of (Id.t * PatVal.t) list
  | DefaultPat

let pp (ppf : Format.formatter) (pat : t) : unit =
  let pp_bind ppf (pn, pv) = Fmt.pf ppf "%a: %a" Id.pp pn PatVal.pp pv in
  match pat.it with
  | ObjPat pbs -> Fmt.pf ppf "{ %a }" Fmt.(list ~sep:comma pp_bind) pbs
  | DefaultPat -> Fmt.string ppf "default"

let str (pat : t) : string = Fmt.str "%a" pp pat [@@inline]

let patval_opt (pat : t) (id : Id.t) : PatVal.t option =
  let find_pn (pn, _) = Id.equal id pn in
  match pat.it with
  | ObjPat pbs -> Option.map snd (List.find_opt find_pn pbs)
  | DefaultPat -> None

let patval_remove (pat : t) (id : Id.t) : t =
  let rec patval_remove' = function
    | [] -> []
    | (pn, _) :: pbs' when Id.equal id pn -> pbs'
    | pb :: pbs' -> pb :: patval_remove' pbs'
  in
  match pat.it with
  | ObjPat pbs -> ObjPat (patval_remove' pbs) @> pat.at
  | DefaultPat -> pat
