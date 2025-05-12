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
open Prelude

type 'a t = ('a -> unit) -> unit

let[@inline] empty : 'a t = fun _ -> ()

let[@inline] return (v : 'a) : 'a t = fun (k : 'a -> unit) -> k v

let[@inline] cons (hd : 'a) (tl : 'a t) =
 fun (k : 'a -> unit) ->
  let () = k hd in
  tl k

let[@inline] map (f : 'a -> 'b) (v : 'a t) =
 fun (k : 'b -> unit) -> v (fun (x : 'a) -> k (f x))

let[@inline] bind (f : 'a -> 'b t) (v : 'a t) =
 fun (k : 'b -> unit) -> v (fun (x : 'a) -> (f x) k)

let of_list (l : 'a list) : 'a t = fun (k : 'a -> unit) -> List.iter k l
