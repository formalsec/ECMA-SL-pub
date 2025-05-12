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

type t = t' Source.t

and t' =
  { mn : Id.t
  ; pxs : Id.t list
  ; s : EStmt.t
  }

let default : unit -> t =
  let dflt = { mn = Id.default (); pxs = []; s = EStmt.default () } @> none in
  fun () -> dflt

let create (mn : Id.t) (pxs : Id.t list) (s : EStmt.t) : t' = { mn; pxs; s }
[@@inline]

let name (m : t) : Id.t = m.it.mn [@@inline]

let name' (m : t) : Id.t' = (name m).it

let params (m : t) : Id.t list = m.it.pxs [@@inline]

let params' (m : t) : Id.t' list = List.map (fun px -> px.it) (params m)

let body (m : t) : EStmt.t = m.it.s [@@inline]

let pp_signature (ppf : Format.formatter) (m : t) : unit =
  let pp_pxs ppf pxs = Fmt.(list ~sep:comma Id.pp) ppf pxs in
  Fmt.pf ppf "@[<h>macro %a(%a)@]" Id.pp m.it.mn pp_pxs m.it.pxs

let pp_simple (ppf : Format.formatter) (m : t) : unit =
  Fmt.pf ppf "%a { ..." pp_signature m

let pp (ppf : Format.formatter) (m : t) : unit =
  Fmt.pf ppf "%a %a" pp_signature m EStmt.pp m.it.s

let str (m : t) : string = Fmt.str "%a" pp m [@@inline]
