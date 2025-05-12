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

module Config = struct
  let log_warns : bool ref = ref true

  let log_debugs : bool ref = ref false

  let out_ppf : Format.formatter ref = ref Fmt.stdout

  let err_ppf : Format.formatter ref = ref Fmt.stderr
end

let stdout (format : ('a, Format.formatter, unit) format) =
  Fmt.pf !Config.out_ppf format
[@@inline]

let stderr (format : ('a, Format.formatter, unit) format) =
  Fmt.pf !Config.err_ppf format
[@@inline]

let fail = Fmt.failwith

module EslLog = struct
  let mk ?(font : Font.t = [ Font.Normal ]) (ppf : Format.formatter)
    (format : ('a, Format.formatter, unit) format) : 'a =
    let pp_text ppf fmt = Fmt.pf ppf "[ecma-sl] %t" fmt in
    let pp_log fmt = Fmt.pf ppf "%a@." (Font.pp font pp_text) fmt in
    Format.kdprintf pp_log format

  let test (test : bool) ?(font : Font.t option) (ppf : Format.formatter)
    (fmt : ('a, Format.formatter, unit) format) =
    if test then (mk ?font ppf) fmt else Format.ifprintf ppf fmt
end

let esl (fmt : ('a, Format.formatter, unit) format) : 'a =
  EslLog.mk !Config.out_ppf fmt
[@@inline]

let error (fmt : ('a, Format.formatter, unit) format) : 'a =
  EslLog.mk ~font:[ Red ] !Config.err_ppf fmt
[@@inline]

let warn (fmt : ('a, Format.formatter, unit) format) : 'a =
  EslLog.test !Config.log_warns ~font:[ Yellow ] !Config.err_ppf fmt
[@@inline]

let debug (fmt : ('a, Format.formatter, unit) format) : 'a =
  EslLog.test !Config.log_debugs ~font:[ Cyan ] !Config.err_ppf fmt
[@@inline]

(** Use continuations to conditionally print to avoid performance hits of
    ifprintf *)
let debug_k : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit =
  let pp fmt = EslLog.mk ~font:[ Cyan ] !Config.err_ppf fmt in
  fun k -> if !Config.log_debugs then k pp

module Redirect = struct
  type mode =
    | Out
    | Err
    | All
    | Shared

  type t =
    { old_out : Format.formatter
    ; old_err : Format.formatter
    ; new_out : Buffer.t option
    ; new_err : Buffer.t option
    }

  let capture (log_ppf : Format.formatter ref) (new_ppf : Buffer.t) : Buffer.t =
    log_ppf := Format.formatter_of_buffer new_ppf;
    new_ppf

  let capture_to ~(out : Buffer.t option) ~(err : Buffer.t option) : t =
    let (old_out, old_err) = (!Config.out_ppf, !Config.err_ppf) in
    let new_out = Option.map (capture Config.out_ppf) out in
    let new_err = Option.map (capture Config.err_ppf) err in
    { old_out; old_err; new_out; new_err }

  let capture (mode : mode) : t =
    let buffer () = Some (Buffer.create 1024) in
    match mode with
    | Out -> capture_to ~out:(buffer ()) ~err:None
    | Err -> capture_to ~err:(buffer ()) ~out:None
    | All -> capture_to ~out:(buffer ()) ~err:(buffer ())
    | Shared ->
      let streams = capture_to ~out:(buffer ()) ~err:None in
      let old_err = !Config.err_ppf in
      Config.err_ppf := !Config.out_ppf;
      { streams with old_err; new_err = None }

  let pp_captured (ppf : Format.formatter) (streams : t) : unit =
    let log ppf buf = Fmt.string ppf (Buffer.contents buf) in
    Option.fold ~none:() ~some:(log ppf) streams.new_out;
    Option.fold ~none:() ~some:(log ppf) streams.new_err

  let restore ?(log : bool = false) (streams : t) : unit =
    let log ppf buf = if log then Fmt.pf ppf "%s@?" (Buffer.contents buf) in
    Config.out_ppf := streams.old_out;
    Config.err_ppf := streams.old_err;
    Option.fold ~none:() ~some:(log !Config.out_ppf) streams.new_out;
    Option.fold ~none:() ~some:(log !Config.err_ppf) streams.new_err
end
