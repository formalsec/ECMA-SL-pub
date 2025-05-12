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

type pos =
  { line : int
  ; col : int
  }

type at =
  { file : string
  ; lpos : pos
  ; rpos : pos
  ; real : bool
  }

type +'a t =
  { it : 'a
  ; at : at
  }

let pos_none : pos = { line = -1; col = -1 }

let none : at = { file = ""; lpos = pos_none; rpos = pos_none; real = false }

let view { it; _ } = it

let equal_pos a b = a.line = b.line && a.col = b.col

let equal a b =
  String.equal a.file b.file
  && equal_pos a.lpos b.lpos
  && equal_pos a.rpos b.rpos
  && Bool.equal a.real b.real

let ( @> ) (it : 'a) (at : at) : 'a t = { it; at } [@@inline]

let ( @?> ) (it : 'a) (at : at) : 'a t = it @> { at with real = false }

let map (f : 'a -> 'b) (x : 'a t) : 'b t = { x with it = f x.it } [@@inline]

let is_none (at : at) : bool = equal at none [@@inline]

let pp_pos (ppf : Format.formatter) (pos : pos) : unit =
  let pp_pos' ppf v = Fmt.(if v = -1 then string ppf "x" else int ppf v) in
  Fmt.pf ppf "%a.%a" pp_pos' pos.line pp_pos' pos.col

let pp_at (ppf : Format.formatter) (at : at) : unit =
  Fmt.pf ppf "%S:%a-%a" at.file pp_pos at.lpos pp_pos at.rpos

let pp (ppf : Format.formatter) (x : 'a t) = Fmt.pf ppf "%a" pp_at x.at
[@@inline]

let str (x : 'a t) : string = Fmt.str "%a" pp x [@@inline]
