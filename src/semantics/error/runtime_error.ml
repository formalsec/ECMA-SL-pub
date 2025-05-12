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
open EslSyntax
module ErrSrc = Error_source
module RtTrace = Error_trace

type msg =
  | Default
  | Custom of string
  | Failure of string
  | Unexpected of string
  | UncaughtExn of string
  | OpEvalExn of string
  | UnknownVar of Id.t'
  | UnknownFunc of Id.t'
  | MissingReturn of Id.t
  | BadNArgs of int * int
  | BadArg of string * Value.t
  | BadVal of string * Value.t
  | BadExpr of string * Value.t
  | BadFuncId of Value.t
  | BadOpArgs of string * Value.t list

module RuntimeErr : Error_type.ERROR_TYPE with type t = msg = struct
  type t = msg

  let header : string = "RuntimeError"

  let font : Font.t = [ Red ]

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') -> String.equal msg1' msg2'
    | (Failure msg1', Failure msg2') -> String.equal msg1' msg2'
    | (Unexpected msg1', Unexpected msg2') -> String.equal msg1' msg2'
    | (UncaughtExn msg1', UncaughtExn msg2') -> String.equal msg1' msg2'
    | (OpEvalExn oplbl1, OpEvalExn oplbl2) -> String.equal oplbl1 oplbl2
    | (UnknownVar x1, UnknownVar x2) -> String.equal x1 x2
    | (UnknownFunc fn1, UnknownFunc fn2) -> String.equal fn1 fn2
    | (MissingReturn fn1, MissingReturn fn2) -> Id.equal fn1 fn2
    | (BadNArgs (npxs1, nargs1), BadNArgs (npxs2, nargs2)) ->
      Int.equal npxs1 npxs2 && Int.equal nargs1 nargs2
    | (BadArg (texp1, v1), BadArg (texp2, v2)) ->
      String.equal texp1 texp2 && Value.equal v1 v2
    | (BadVal (texp1, v1), BadVal (texp2, v2)) ->
      String.equal texp1 texp2 && Value.equal v1 v2
    | (BadExpr (texp1, v1), BadExpr (texp2, v2)) ->
      String.equal texp1 texp2 && Value.equal v1 v2
    | (BadFuncId v1, BadFuncId v2) -> Value.equal v1 v2
    | _ -> false

  let pp (ppf : Format.formatter) (msg : t) : unit =
    match msg with
    | Default -> Fmt.pf ppf "Generic runtime error."
    | Custom msg' -> Fmt.pf ppf "%s" msg'
    | Failure msg -> Fmt.pf ppf "Failure: %s" msg
    | Unexpected msg -> Fmt.pf ppf "Unexpected %s." msg
    | UncaughtExn msg -> Fmt.pf ppf "Uncaught exception: %s" msg
    | OpEvalExn oplbl -> Fmt.pf ppf "Operator evaluation exception: %s" oplbl
    | UnknownVar x -> Fmt.pf ppf "Cannot find variable '%s'." x
    | UnknownFunc fn -> Fmt.pf ppf "Cannot find function '%s'." fn
    | MissingReturn fn -> Fmt.pf ppf "Missing return in function '%a'." Id.pp fn
    | BadNArgs (npxs, nargs) ->
      Fmt.pf ppf "Expected %d arguments, but got %d." npxs nargs
    | BadArg (texp, v) ->
      Fmt.pf ppf "Expecting argument of type '%s' but got '%a'." texp Value.pp v
    | BadVal (texp, v) ->
      Fmt.pf ppf "Expecting %s value, but got '%a'." texp Value.pp v
    | BadExpr (texp, v) ->
      Fmt.pf ppf "Expecting %s expression, but got '%a'." texp Value.pp v
    | BadFuncId v ->
      Fmt.pf ppf "Expecting a function identifier, but got '%a'." Value.pp v
    | BadOpArgs (texp, vs) when List.length vs = 1 ->
      Fmt.pf ppf "Expecting argument of type '%s', but got '%a'." texp
        Fmt.(list ~sep:comma Value.pp)
        vs
    | BadOpArgs (texp, vs) ->
      Fmt.pf ppf "Expecting arguments of types '%s', but got '(%a)'." texp
        Fmt.(list ~sep:comma Value.pp)
        vs

  let str (msg : t) : string = Fmt.str "%a" pp msg [@@inline]
end

type t =
  { msgs : msg list
  ; src : ErrSrc.t
  ; trace : RtTrace.t option
  }

exception Error of t

let raise (err : t) : 'a = raise_notrace (Error err) [@@inline]

let create ?(src : ErrSrc.t = Source.none) (msgs : msg list) : t =
  { msgs; src; trace = None }
[@@inline]

let throw ?(src : ErrSrc.t = Source.none) (msg : msg) : 'a =
  raise @@ create ~src [ msg ]
[@@inline]

let src (err : t) : ErrSrc.t = err.src [@@inline]

let trace (err : t) : RtTrace.t option = err.trace [@@inline]

let set_src (src : ErrSrc.t) (err : t) : t = { err with src } [@@inline]

let set_trace (tr : RtTrace.t) (err : t) : t = { err with trace = Some tr }
[@@inline]

let push (msg : msg) (err : t) : t = { err with msgs = msg :: err.msgs }
[@@inline]

let pp code (ppf : Format.formatter) (err : t) : unit =
  let module MsgFmt = Error_type.ErrorTypeFmt (RuntimeErr) in
  let module ErrSrcFmt = ErrSrc.ErrSrcFmt (RuntimeErr) in
  let module RtTraceFmt = RtTrace.RtTraceFmt (RuntimeErr) in
  Fmt.pf ppf "%a%a%a" MsgFmt.pp err.msgs ErrSrcFmt.pp (code, err.src)
    (Fmt.option RtTraceFmt.pp) err.trace

let str code (err : t) = Fmt.str "%a" (pp code) err [@@inline]
