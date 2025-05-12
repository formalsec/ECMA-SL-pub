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

open EslBase

type t = (Id.t', Func.t) Hashtbl.t

let default () : t = Hashtbl.create !Base.default_hashtbl_sz

let create (fs : Func.t list) : t =
  let p = default () in
  List.iter (fun f -> Hashtbl.replace p (Func.name' f) f) fs;
  p

let funcs (p : t) : (Id.t', Func.t) Hashtbl.t = p [@@inline]

let func (p : t) (fn : Id.t') : Func.t option = Hashtbl.find_opt p fn [@@inline]

let add_func (p : t) (fn : Id.t') (f : Func.t) : unit = Hashtbl.replace p fn f
[@@inline]

let pp (ppf : Format.formatter) (p : t) : unit =
  let pp_func ppf (_, f) = Func.pp ppf f in
  let sep ppf () = Fmt.pf ppf ";@\n" in
  Fmt.(hashtbl ~sep pp_func) ppf p

let str (p : t) : string = Fmt.str "%a" pp p [@@inline]
