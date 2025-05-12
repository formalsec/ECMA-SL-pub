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

(* This is very tricky, -0 has to not be an int *)
let is_int (f : float) : bool =
  let f' = float_of_int (int_of_float f) in
  Float.equal f f' && Float.equal (copysign 1.0 f) (copysign 1.0 f')

let is_normal (f : float) = not (Float.is_infinite f || Float.is_nan f)

let to_int n =
  match classify_float n with
  | FP_nan -> 0.
  | FP_infinite -> n
  | FP_zero -> n
  | FP_normal | FP_subnormal ->
    (if Float.Infix.(n < 0.) then -1. else 1.) *. floor (abs_float n)

let to_int32 n =
  let open Float.Infix in
  match classify_float n with
  | FP_normal | FP_subnormal ->
    let i32 = 2. ** 32. in
    let i31 = 2. ** 31. in
    let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
    let int32bit =
      let smod = mod_float posint i32 in
      if smod < 0. then smod +. i32 else smod
    in
    if int32bit >= i31 then int32bit -. i32 else int32bit
  | _ -> 0.

let to_uint32 n =
  let open Float.Infix in
  match classify_float n with
  | FP_normal | FP_subnormal ->
    let i32 = 2. ** 32. in
    let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
    let int32bit =
      let smod = mod_float posint i32 in
      if smod < 0. then smod +. i32 else smod
    in
    int32bit
  | _ -> 0.

let to_uint16 n =
  let open Float.Infix in
  match classify_float n with
  | FP_normal | FP_subnormal ->
    let i16 = 2. ** 16. in
    let posint = (if n < 0. then -1. else 1.) *. floor (abs_float n) in
    let int16bit =
      let smod = mod_float posint i16 in
      if smod < 0. then smod +. i16 else smod
    in
    int16bit
  | _ -> 0.

let modulo_32 x =
  let r = mod_float x 32. in
  if Float.Infix.(x < 0.) then r +. 32. else r

let int32_bitwise_not x = Int32.to_float (Int32.lognot (Int32.of_float x))

let int32_bitwise_and x y =
  Int32.to_float (Int32.logand (Int32.of_float x) (Int32.of_float y))

let int32_bitwise_or x y =
  Int32.to_float (Int32.logor (Int32.of_float x) (Int32.of_float y))

let int32_bitwise_xor x y =
  Int32.to_float (Int32.logxor (Int32.of_float x) (Int32.of_float y))

let int32_left_shift x y =
  let l = Int32.of_float x in
  let r = int_of_float y mod 32 in
  Int32.to_float (Int32.shift_left l r)

let int32_right_shift x y =
  let l = Int32.of_float x in
  let r = int_of_float y mod 32 in
  Int32.to_float (Int32.shift_right l r)

let uint32_right_shift x y =
  let open Float.Infix in
  let i31 = 2. ** 31. in
  let i32 = 2. ** 32. in
  let signedx = if x >= i31 then x -. i32 else x in
  let left = Int32.of_float signedx in
  let right = int_of_float y mod 32 in
  let r = Int32.to_float (Int32.shift_right_logical left right) in
  if r < 0. then r +. i32 else r

(* This is intended to work on positive floats! *)
let string_of_pos_float num =
  let open Float.Infix in
  (* Is the number an integer? *)
  let inum = int_of_float num in
  if is_int num then string_of_int inum (* It is not an integer *)
  else if
    num > 1e+9 && num < 1e+21
    (*  %.0f fails bench/test262/tests/built-ins/String/prototype/split/S15.5.4.14_A1_T18.js *)
  then Fmt.str "%.0f" num
  else if 1e-5 <= num && num < 1e-4 then
    let s = string_of_float (num *. 10.) in
    let len = String.length s in
    Fmt.str "0.0%s" (String.sub s 2 (len - 2))
  else if 1e-6 <= num && num < 1e-5 then
    let s = string_of_float (num *. 100.) in
    let len = String.length s in
    Fmt.str "0.00%s" (String.sub s 2 (len - 2))
  else
    let re = Re.regexp "e\\([-+]\\)0" in
    (* e+0 -> e+ *)
    Re.replace_first re "e\\1" (string_of_float num)

let rec float_to_string_inner n =
  let open Float.Infix in
  if Float.is_nan n then "NaN"
  else if n = 0.0 || n = -0.0 then "0"
  else if n < 0.0 then Fmt.str "-%s" (float_to_string_inner (-.n))
  else if n = Float.infinity then "Infinity"
  else string_of_pos_float n

(*
   "1.2343434e+15" -> appropriate float
*)
let parse_efloat (f_str : string) : float =
  let i =
    match String.rindex_opt f_str 'e' with Some i -> i | None -> assert false
  in
  let b =
    match float_of_string_opt (String.sub f_str 0 i) with
    | Some s -> s
    | None -> assert false
  in
  let len' = String.length f_str - i - 2 in
  let exp =
    match float_of_string_opt (String.sub f_str (i + 2) len') with
    | Some f -> f
    | None -> assert false
  in
  b *. (10. ** exp)

let rec count_digits (f : float) : int =
  if is_int f then 0 else 1 + count_digits (f *. 10.)
