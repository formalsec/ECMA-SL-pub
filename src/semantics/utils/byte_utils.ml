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
open EslBase

let float64_to_le_bytes (x : float) : int64 list =
  let v = Int64.bits_of_float x in
  let lst = ref [] in
  for i = 0 to 7 do
    lst := !lst @ [ Int64.logand 255L (Int64.shift_right v (i * 8)) ]
  done;
  !lst

let float64_to_be_bytes (x : float) : int64 list =
  let v = Int64.bits_of_float x in
  let lst = ref [] in
  for i = 0 to 7 do
    lst := Int64.logand 255L (Int64.shift_right v (i * 8)) :: !lst
  done;
  !lst

let float32_to_le_bytes (x : float) : int32 list =
  let v = Int32.bits_of_float x in
  let lst = ref [] in
  for i = 0 to 3 do
    lst := !lst @ [ Int32.logand 255l (Int32.shift_right v (i * 8)) ]
  done;
  !lst

let float32_to_be_bytes (x : float) : int32 list =
  let v = Int32.bits_of_float x in
  let lst = ref [] in
  for i = 0 to 3 do
    lst := Int32.logand 255l (Int32.shift_right v (i * 8)) :: !lst
  done;
  !lst

let int_to_be_bytes ((x, n) : float * int) : int list =
  Log.debug "debug 42: int_to_be_bytes x: %f, n: %d\n" x n;
  let rec loop (lst : int list) (value : int) (j : int) =
    Log.debug "debug 42: int_to_be_bytes value: %d, j: %d\n" value j;
    if j < n then
      loop ([ Int.logand value 255 ] @ lst) (Int.shift_right value 8) (j + 1)
    else lst
  in
  loop [] (Float.to_int x) 0

let uint_from_le_bytes ((arr, n) : int array * int) : float =
  Log.debug "debug 42: uint_from_le_bytes n: %d\n" n;
  let len = Array.length arr in
  let rec loop (value : int) (j : int) =
    Log.debug "debug 42: uint_from_le_bytes value: %d, j: %d\n" value j;
    if j >= 0 then loop ((value * 256) + Array.get arr j) (j - 1) else value
  in
  Int.to_float (loop 0 (len - 1))

let int_from_le_bytes ((arr, n) : int array * int) : float =
  let value = uint_from_le_bytes (arr, n) in
  (* check sign of value *)
  if Float.Infix.(value < 2. ** Int.to_float ((8 * n) - 1)) then value
  else value -. (2. ** Int.to_float (8 * n))

let float64_from_le_bytes (bytes : int64 array) : float =
  let res = ref 0L in
  for i = 0 to 7 do
    res := Int64.add !res (Int64.shift_left (Array.get bytes i) (i * 8))
  done;
  Int64.float_of_bits !res

let float64_from_be_bytes (bytes : int64 array) : float =
  let res = ref 0L in
  for i = 0 to 7 do
    res := Int64.add !res (Int64.shift_left (Array.get bytes i) ((7 - i) * 8))
  done;
  Int64.float_of_bits !res

let float32_from_le_bytes (bytes : int32 array) : float =
  let res = ref 0l in
  for i = 0 to 3 do
    res := Int32.add !res (Int32.shift_left (Array.get bytes i) (i * 8));
    Log.debug "in float32_from_le_bytes loop, i = %d; byte = %d; res = %f\n" i
      (Int32.to_int (Array.get bytes i))
      (Int32.float_of_bits !res)
  done;
  Int32.float_of_bits !res

let float32_from_be_bytes (bytes : int32 array) : float =
  let res = ref 0l in
  for i = 0 to 3 do
    res := Int32.add !res (Int32.shift_left (Array.get bytes i) ((3 - i) * 8))
  done;
  Int32.float_of_bits !res
