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
  { fn : Id.t
  ; pxs : Id.t list
  ; s : Stmt.t
  }

let default : unit -> t =
  let dflt = { fn = Id.default (); pxs = []; s = Stmt.default () } @> none in
  fun () -> dflt

let create (fn : Id.t) (pxs : Id.t list) (s : Stmt.t) : t' = { fn; pxs; s }
[@@inline]

let name (f : t) : Id.t = f.it.fn [@@inline]

let name' (f : t) : Id.t' = (name f).it

let params (f : t) : Id.t list = f.it.pxs [@@inline]

let params' (f : t) : Id.t' list = List.map (fun px -> px.it) (params f)

let body (f : t) : Stmt.t = f.it.s [@@inline]

let pp_signature (ppf : Format.formatter) (f : t) : unit =
  let pp_pxs ppf pxs = Fmt.(list ~sep:comma Id.pp) ppf pxs in
  Fmt.pf ppf "@[<h>function %a(%a)@]" Id.pp f.it.fn pp_pxs f.it.pxs

let pp_simple (ppf : Format.formatter) (f : t) : unit =
  Fmt.pf ppf "%a { ..." pp_signature f

let pp (ppf : Format.formatter) (f : t) : unit =
  Fmt.pf ppf "%a %a" pp_signature f Stmt.pp f.it.s

let str (f : t) : string = Fmt.str "%a" pp f [@@inline]
