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

open Ecma_sl

module DebugLvl = struct
  type t =
    | None
    | Warn
    | Full

  let all : t list = [ None; Warn; Full ]

  let pp (ppf : Format.formatter) (lvl : t) : unit =
    match lvl with
    | None -> Fmt.string ppf "none"
    | Warn -> Fmt.string ppf "warn"
    | Full -> Fmt.string ppf "full"

  let str (lvl : t) : string = Fmt.str "%a" pp lvl

  let args (lvls : t list) : (string * t) list =
    List.map (fun lvl -> (str lvl, lvl)) lvls

  let value (lvl : t) : int = match lvl with None -> 0 | Warn -> 1 | Full -> 2

  let ( < ) (lvl1 : t) (lvl2 : t) : bool = Stdlib.(value lvl1 < value lvl2)

  let ( > ) (lvl1 : t) (lvl2 : t) : bool = Stdlib.(value lvl1 > value lvl2)

  let ( <= ) (lvl1 : t) (lvl2 : t) : bool = Stdlib.(value lvl1 <= value lvl2)

  let ( >= ) (lvl1 : t) (lvl2 : t) : bool = Stdlib.(value lvl1 >= value lvl2)
end

module Lang = struct
  type t =
    | Auto
    | JS
    | ESL
    | CESL
    | CESLUnattached
    | TestReport
    | TestSummary

  let pp (ppf : Format.formatter) (lang : t) : unit =
    match lang with
    | Auto -> Fmt.string ppf "auto"
    | JS -> Fmt.string ppf ".js"
    | ESL -> Fmt.string ppf ".esl"
    | CESL -> Fmt.string ppf ".cesl"
    | CESLUnattached -> Fmt.string ppf ".cesl"
    | TestReport -> Fmt.string ppf ".trp"
    | TestSummary -> Fmt.string ppf ".tsmry"

  let str (lang : t) : string = Fmt.str "%a" pp lang

  let description (lang : t) : string =
    match lang with
    | Auto -> "auto"
    | JS -> "js"
    | ESL -> "esl"
    | CESL -> "cesl"
    | CESLUnattached -> "cesl-unattached"
    | TestReport -> "trp"
    | TestSummary -> "tsmry"

  let args (langs : t list) : (string * t) list =
    List.map (fun lang -> (description lang, lang)) langs

  let valid_langs (valid_langs : t list) (user_lang : t) : t list =
    match user_lang with Auto -> valid_langs | _ -> [ user_lang ]

  let resolve_file_ext (valid_langs : t list) (fpath : Fpath.t) : t option =
    match Fpath.get_ext fpath with
    | ".js" when List.mem JS valid_langs -> Some JS
    | ".esl" when List.mem ESL valid_langs -> Some ESL
    | ".cesl" when List.mem CESL valid_langs -> Some CESL
    | ".cesl" when List.mem CESLUnattached valid_langs -> Some CESLUnattached
    | ".trp" when List.mem TestReport valid_langs -> Some TestReport
    | ".tsmry" when List.mem TestSummary valid_langs -> Some TestSummary
    | _ -> None

  let resolve_file_lang ?(warn : bool = true) (langs : t list) (fpath : Fpath.t)
    : t option =
    let lang = resolve_file_ext langs fpath in
    if Option.is_none lang && warn then begin
      let sep ppf () = Fmt.string ppf " | " in
      Log.warn "expecting file extensions: %a" Fmt.(list ~sep pp) langs
    end;
    lang
end

module InterpTracer = struct
  type t =
    | None
    | Call
    | Step
    | Full
    | Core

  let all () : t list = [ None; Call; Step; Full; Core ]

  let pp (ppf : Format.formatter) (tracer : t) : unit =
    match tracer with
    | None -> Fmt.string ppf "none"
    | Call -> Fmt.string ppf "call"
    | Step -> Fmt.string ppf "step"
    | Full -> Fmt.string ppf "full"
    | Core -> Fmt.string ppf "core"

  let str (tracer : t) : string = Fmt.str "%a" pp tracer

  let args (tracers : t list) : (string * t) list =
    List.map (fun tracer -> (str tracer, tracer)) tracers
end

module InterpProfiler = struct
  type t =
    | None
    | Time
    | Full

  let all () : t list = [ None; Time; Full ]

  let equal a b =
    match (a, b) with
    | (None, None) | (Time, Time) | (Full, Full) -> true
    | ((None | Time | Full), _) -> false

  let pp (ppf : Format.formatter) (profiler : t) : unit =
    match profiler with
    | None -> Fmt.string ppf "none"
    | Time -> Fmt.string ppf "time"
    | Full -> Fmt.string ppf "full"

  let str (profiler : t) : string = Fmt.str "%a" pp profiler

  let args (profilers : t list) : (string * t) list =
    List.map (fun profiler -> (str profiler, profiler)) profilers
end

module JSInterp = struct
  type t =
    | Main
    | Latest
    | ECMARef5
    | ECMARef6
    | ECMARef6Sym

  let all () : t list = [ Main; Latest; ECMARef5; ECMARef6 ]

  let pp (ppf : Format.formatter) (version : t) : unit =
    match version with
    | Main -> Fmt.string ppf "main"
    | Latest -> Fmt.string ppf "latest"
    | ECMARef5 -> Fmt.string ppf "ecmaref5"
    | ECMARef6 -> Fmt.string ppf "ecmaref6"
    | ECMARef6Sym -> Fmt.string ppf "ecmaref6-sym"

  let str (version : t) : string = Fmt.str "%a" pp version

  let args (versions : t list) : (string * t) list =
    List.map (fun version -> (str version, version)) versions

  let interp (version : t) : string =
    match version with
    | Main -> Share.es6_config ()
    | Latest -> Share.es6_config ()
    | ECMARef5 -> Share.es5_config ()
    | ECMARef6 -> Share.es6_config ()
    | ECMARef6Sym -> Share.es6_sym_config ()
end

module JSTest = struct
  type t =
    | Auto
    | Simple
    | Test262

  let all () : t list = [ Auto; Simple; Test262 ]

  let pp (ppf : Format.formatter) (kind : t) : unit =
    match kind with
    | Auto -> Fmt.string ppf "auto"
    | Simple -> Fmt.string ppf "simple"
    | Test262 -> Fmt.string ppf "test262"

  let str (kind : t) : string = Fmt.str "%a" pp kind

  let args (kinds : t list) : (string * t) list =
    List.map (fun kind -> (str kind, kind)) kinds
end
