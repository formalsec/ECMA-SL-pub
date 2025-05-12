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

type msg =
  | Default
  | Custom of string
  | UnknownDependency of Id.t
  | CyclicDependency of Id.t
  | DuplicatedField of Id.t
  | DuplicatedParam of Id.t
  | DuplicatedTDef of Id.t
  | DuplicatedFunc of Id.t
  | DuplicatedMacro of Id.t
  | UnknownMacro of Id.t
  | BadNArgs of int * int
  | DuplicatedSwitchCase of Value.t
  | DuplicatedTField of Id.t
  | DuplicatedSigmaDiscriminant of EType.t
  | MissingSigmaDiscriminant of Id.t
  | UnexpectedSigmaDiscriminant
  | UnexpectedSigmaCase

module CompileErr : Error_type.ERROR_TYPE with type t = msg = struct
  type t = msg

  let header : string = "CompileError"

  let font : Font.t = [ Red ]

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') -> String.equal msg1' msg2'
    | (UnknownDependency file1, UnknownDependency file2) -> Id.equal file1 file2
    | (CyclicDependency file1, CyclicDependency file2) -> Id.equal file1 file2
    | (DuplicatedField fn1, DuplicatedField fn2) -> Id.equal fn1 fn2
    | (DuplicatedParam px1, DuplicatedParam px2) -> Id.equal px1 px2
    | (DuplicatedTDef tn1, DuplicatedTDef tn2) -> Id.equal tn1 tn2
    | (DuplicatedFunc fn1, DuplicatedFunc fn2) -> Id.equal fn1 fn2
    | (DuplicatedMacro mn1, DuplicatedMacro mn2) -> Id.equal mn1 mn2
    | (UnknownMacro mn1, UnknownMacro mn2) -> Id.equal mn1 mn2
    | (BadNArgs (npxs1, nargs1), BadNArgs (npxs2, nargs2)) ->
      Int.equal npxs1 npxs2 && Int.equal nargs1 nargs2
    | (DuplicatedSwitchCase v1, DuplicatedSwitchCase v2) -> Value.equal v1 v2
    | (DuplicatedTField fn1, DuplicatedTField fn2) -> Id.equal fn1 fn2
    | (DuplicatedSigmaDiscriminant lt1, DuplicatedSigmaDiscriminant lt2) ->
      EType.equal lt1 lt2
    | (MissingSigmaDiscriminant dsc1, MissingSigmaDiscriminant dsc2) ->
      Id.equal dsc1 dsc2
    | (UnexpectedSigmaDiscriminant, UnexpectedSigmaDiscriminant) -> true
    | (UnexpectedSigmaCase, UnexpectedSigmaCase) -> true
    | _ -> false

  let pp (ppf : Format.formatter) (msg : t) : unit =
    match msg with
    | Default -> Fmt.pf ppf "Generic compilation error."
    | Custom msg' -> Fmt.pf ppf "%s" msg'
    | UnknownDependency file ->
      Fmt.pf ppf "Cannot find dependency '%a'." Id.pp file
    | CyclicDependency file ->
      Fmt.pf ppf "Cyclic dependency of file '%a'." Id.pp file
    | DuplicatedField fn ->
      Fmt.pf ppf "Duplicated field name '%a' for object literal." Id.pp fn
    | DuplicatedParam px ->
      Fmt.pf ppf "Duplicated parameter '%a' for function definition." Id.pp px
    | DuplicatedTDef tn ->
      Fmt.pf ppf "Duplicated definition for typedef '%a'." Id.pp tn
    | DuplicatedFunc fn ->
      Fmt.pf ppf "Duplicated definition for function '%a'." Id.pp fn
    | DuplicatedMacro mn ->
      Fmt.pf ppf "Duplicated definition for macro '%a'." Id.pp mn
    | UnknownMacro mn -> Fmt.pf ppf "Cannot find macro '%a'." Id.pp mn
    | BadNArgs (npxs, nargs) ->
      Fmt.pf ppf "Expected %d arguments, but got %d." npxs nargs
    | DuplicatedSwitchCase v ->
      Fmt.pf ppf "Duplicated case value '%a' for switch statement." Value.pp v
    | DuplicatedTField fn ->
      Fmt.pf ppf "Duplicated field name '%a' for object type." Id.pp fn
    | DuplicatedSigmaDiscriminant lt ->
      Fmt.pf ppf "Duplicated discriminant '%a' for multiple sigma cases."
        EType.pp lt
    | MissingSigmaDiscriminant dsc ->
      Fmt.pf ppf "Missing discriminant '%a' from sigma case." Id.pp dsc
    | UnexpectedSigmaDiscriminant ->
      Fmt.pf ppf "Expecting literal type for sigma case discriminant."
    | UnexpectedSigmaCase ->
      Fmt.pf ppf "Expecting union of object types for sigma type."

  let str (msg : t) : string = Fmt.str "%a" pp msg
end

type t =
  { msgs : msg list
  ; src : ErrSrc.t
  }

exception Error of t

let raise (err : t) : 'a = raise_notrace (Error err)

let create ?(src : ErrSrc.t = Source.none) (msgs : msg list) : t = { msgs; src }

let throw ?(src : ErrSrc.t = Source.none) (msg : msg) : 'a =
  raise @@ create ~src [ msg ]

let push (msg : msg) (err : t) : t = { err with msgs = msg :: err.msgs }

let src (err : t) : ErrSrc.t = err.src

let set_src (src : ErrSrc.t) (err : t) : t = { err with src }

let pp code (ppf : Format.formatter) (err : t) : unit =
  let module MsgFmt = Error_type.ErrorTypeFmt (CompileErr) in
  let module ErrSrcFmt = ErrSrc.ErrSrcFmt (CompileErr) in
  Fmt.pf ppf "%a%a" MsgFmt.pp err.msgs ErrSrcFmt.pp (code, err.src)

let str code (err : t) = Fmt.str "%a" (pp code) err
