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

type 'sl t = (string, 'sl) Hashtbl.t

let create (var_vals : (string * 'sl) list) : 'sl t =
  List.to_seq var_vals |> Hashtbl.of_seq

let get (store : 'sl t) (var : string) : 'sl =
  match Hashtbl.find_opt store var with Some v -> v | None -> assert false

let get_opt (store : 'sl t) (var : string) : 'sl option =
  Hashtbl.find_opt store var

let set (store : 'sl t) (var : string) (v : 'sl) : unit =
  Hashtbl.replace store var v

let str (sl_printer : 'sl -> string) (store : 'sl t) : string =
  let binding_str x v = Fmt.str "(%s, %s)" x (sl_printer v) in
  let store_str_f x v acc = binding_str x v :: acc in
  Hashtbl.fold store_str_f store [] |> String.concat ", "
