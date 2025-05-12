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

(*YYYY-MM-DDTHH:mm:ss.sssZ*)
let is_final_index str i =
  Log.debug "debug 42: is_final_index %s, %d\n" str i;
  Log.debug "debug 42: is_final_index %b \n" (String.length str = i);
  String.length str = i

let cl2s cl = String.concat "" (List.map (String.make 1) cl)

let is_timezone str i =
  let result =
    Char.code (String.get str i) <= Char.code 'Z'
    && Char.code (String.get str i) >= Char.code 'A'
  in
  Log.debug "debug 42: is_timezone %s %d %b \n" str i result;
  result

let parse_segment (str : string) (i : int) (prev : char) (delim : char) :
  (float * int) option =
  Log.debug "debug 42: parse_segment %s, %d %c %c\n" str i prev delim;
  let len = String.length str in
  if String.length str = i + 1 && Char.equal (String.get str i) prev then None
  else
    let rec loop (chrs : char list) (j : int) =
      if j >= len || Char.equal (String.get str j) delim then Some (chrs, j)
      else
        let cj = String.get str j in
        Log.debug "debug 42: parse_segment char: %c\n" cj;
        if Char.code cj <= Char.code '9' && Char.code cj >= Char.code '0' then
          loop (chrs @ [ cj ]) (j + 1)
        else if Char.code cj <= Char.code 'Z' && Char.code cj >= Char.code 'A'
        then Some (chrs, j)
        else None
    in
    match loop [] (i + 1) with
    | None -> None
    | Some ([], _) -> None
    | Some (chrs, j) ->
      let str_number = cl2s chrs in
      let number =
        match float_of_string_opt str_number with
        | Some f -> f
        | None -> assert false
      in
      Log.debug "debug42: got number: %f\n" number;
      Some (number, j)

let parse_year (str : string) (i : int) : (float * int) option =
  Log.debug "debug 42: parse_year %s, %d\n" str i;
  let len = String.length str in
  let rec loop (chrs : char list) (j : int) =
    if j >= len || Char.equal (String.get str j) '-' then Some (chrs, j)
    else
      let cj = String.get str j in
      Log.debug "debug 42: parse_year char: %c\n" cj;
      if Char.code cj <= Char.code '9' && Char.code cj >= Char.code '0' then
        loop (chrs @ [ cj ]) (j + 1)
      else None
  in
  match loop [] i with
  | None -> None
  | Some (chrs, j) ->
    let str_year = cl2s chrs in
    let year =
      match float_of_string_opt str_year with
      | Some f -> f
      | None -> assert false
    in
    Log.debug "debug42: got year: %f\n" year;
    Some (year, j)

let parse_month (str : string) (i : int) : (float * int) option =
  parse_segment str i '-' '-'

let parse_day (str : string) (i : int) : (float * int) option =
  parse_segment str i '-' 'T'

let parse_hour (str : string) (i : int) : (float * int) option =
  parse_segment str i 'T' ':'

let parse_min (str : string) (i : int) : (float * int) option =
  parse_segment str i ':' ':'

let parse_sec (str : string) (i : int) : (float * int) option =
  parse_segment str i ':' '.'

let parse_msec (str : string) (i : int) : (float * int) option =
  parse_segment str i '.' 'Z' (* TODO *)

let parse_time_zone (str : string) (i : int) : string option =
  Some (Fmt.str "%s%d" str i)

let parse_sth (str : string) (fs : (string -> int -> (float * int) option) list)
  : (float list * int) option =
  Log.debug "debug 42: parse_sth %s\n" str;
  List.fold_left
    (fun ac f ->
      match ac with
      | None -> None
      | Some (ns, i) -> (
        let ret = f str i in
        Log.debug "debug 42: parse_sth indice: %d\n" i;
        match ret with
        | None when is_final_index str i || is_timezone str i ->
          Log.debug "debug 42: parse_sth in first case\n";
          Some (ns @ [ Float.nan ], i)
        | None ->
          Log.debug "debug 42: parse_sth in none case\n";
          None
        | Some (n, i') ->
          Log.debug "debug 42: parse_sth in last case\n";
          Some (ns @ [ n ], i') ) )
    (Some ([], 0))
    fs

(*YYYY-MM-DDTHH:mm:ss.sssZ*)

let parse_date (date_str : string) : (float list * string) option =
  let ret =
    parse_sth date_str
      [ parse_year
      ; parse_month
      ; parse_day
      ; parse_hour
      ; parse_min
      ; parse_sec
      ; parse_msec
      ]
  in
  (*Log.debug "parsed year: %f\n" (List.nth ret 0); *)
  match ret with
  | None -> None
  | Some (ns, i) -> (
    let zone = parse_time_zone date_str i in
    match zone with None -> None | Some zone -> Some (ns, zone) )
