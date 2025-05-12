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
  | UnknownVar of Id.t'
  | MissingField of Id.t
  | ExtraField of Id.t
  | IncompatibleField of Id.t
  | IncompatibleOptionalField of Id.t
  | IncompatibleSummaryField of Id.t
  | MissingSummaryField of EType.t
  | ExtraSummaryField
  | NExpectedElements of int * int
  | IncompatibleElement of int
  | IncompatibleSigmaDiscriminant
  | MissingSigmaCase of EType.t
  | ExtraSigmaCase of EType.t
  | IncompatibleSigmaCase of EType.t
  | MissingSigmaCaseDiscriminant of Id.t
  | UnknownSigmaCaseDiscriminant of EType.t
  | BadCongruency of EType.t * EType.t
  | BadSubtyping of EType.t * EType.t
  | BadOperand of EType.t * EType.t
  | BadReturn of EType.t * EType.t

module TypingErr : Error_type.ERROR_TYPE with type t = msg = struct
  type t = msg

  let header : string = "TypeError"

  let font : Font.t = [ Red ]

  let equal (msg1 : t) (msg2 : t) : bool =
    match (msg1, msg2) with
    | (Default, Default) -> true
    | (Custom msg1', Custom msg2') -> String.equal msg1' msg2'
    | (UnknownVar x1, UnknownVar x2) -> String.equal x1 x2
    | (MissingField fn1, MissingField fn2) -> Id.equal fn1 fn2
    | (ExtraField fn1, ExtraField fn2) -> Id.equal fn1 fn2
    | (IncompatibleField fn1, IncompatibleField fn2) -> Id.equal fn1 fn2
    | (IncompatibleOptionalField fn1, IncompatibleOptionalField fn2) ->
      Id.equal fn1 fn2
    | (IncompatibleSummaryField fn1, IncompatibleSummaryField fn2) ->
      Id.equal fn1 fn2
    | (MissingSummaryField ft1, MissingSummaryField ft2) -> EType.equal ft1 ft2
    | (ExtraSummaryField, ExtraSummaryField) -> true
    | (NExpectedElements (nref1, nsrc1), NExpectedElements (nref2, nsrc2)) ->
      Int.equal nref1 nref2 && Int.equal nsrc1 nsrc2
    | (IncompatibleElement i1, IncompatibleElement i2) -> Int.equal i1 i2
    | (IncompatibleSigmaDiscriminant, IncompatibleSigmaDiscriminant) -> true
    | (MissingSigmaCase tdsc1, MissingSigmaCase tdsc2) ->
      EType.equal tdsc1 tdsc2
    | (ExtraSigmaCase tdsc1, ExtraSigmaCase tdsc2) -> EType.equal tdsc1 tdsc2
    | (IncompatibleSigmaCase tdsc1, IncompatibleSigmaCase tdsc2) ->
      EType.equal tdsc1 tdsc2
    | (MissingSigmaCaseDiscriminant dsc1, MissingSigmaCaseDiscriminant dsc2) ->
      Id.equal dsc1 dsc2
    | (UnknownSigmaCaseDiscriminant t1, UnknownSigmaCaseDiscriminant t2) ->
      EType.equal t1 t2
    | (BadCongruency (tref1, tsrc1), BadCongruency (tref2, tsrc2)) ->
      EType.equal tref1 tref2 && EType.equal tsrc1 tsrc2
    | (BadSubtyping (tref1, tsrc1), BadSubtyping (tref2, tsrc2)) ->
      EType.equal tref1 tref2 && EType.equal tsrc1 tsrc2
    | (BadOperand (tpx1, targ1), BadOperand (tpx2, targ2)) ->
      EType.equal tpx1 tpx2 && EType.equal targ1 targ2
    | (BadReturn (tret1, tsrc1), BadReturn (tret2, tsrc2)) ->
      EType.equal tret1 tret2 && EType.equal tsrc1 tsrc2
    | _ -> false

  let pp (ppf : Format.formatter) (msg : t) : unit =
    match msg with
    | Default -> Fmt.pf ppf "Generic type error."
    | Custom msg' -> Fmt.pf ppf "%s" msg'
    | UnknownVar x -> Fmt.pf ppf "Cannot find variable '%s'." x
    | MissingField fn ->
      Fmt.pf ppf "Field '%a' is missing from the object's type." Id.pp fn
    | ExtraField fn ->
      Fmt.pf ppf "Field '%a' is not defined in the object's type." Id.pp fn
    | IncompatibleField fn ->
      Fmt.pf ppf "Types of field '%a' are incompatible." Id.pp fn
    | IncompatibleOptionalField fn ->
      Fmt.pf ppf "Types of optional field '%a' are incompatible." Id.pp fn
    | IncompatibleSummaryField fn ->
      Fmt.pf ppf "Type of field '%a' is incompatible with the summary type."
        Id.pp fn
    | MissingSummaryField ft ->
      Fmt.pf ppf "Summary field '%a' is missing from the object's type."
        EType.pp ft
    | ExtraSummaryField ->
      Fmt.pf ppf "Summary field is not defined in the object's type."
    | NExpectedElements (ntsref, ntssrc) ->
      Fmt.pf ppf "Expecting %d elements, but %d were provided." ntsref ntssrc
    | IncompatibleElement i ->
      Fmt.pf ppf "Types of the %s element are incompatible."
        (Base.ordinal_suffix i)
    | IncompatibleSigmaDiscriminant ->
      Fmt.pf ppf "Discriminant fields are incompatible."
    | MissingSigmaCase tdsc ->
      Fmt.pf ppf
        "Sigma case of discriminant '%a' is missing from the sigma type."
        EType.pp tdsc
    | ExtraSigmaCase tdsc ->
      Fmt.pf ppf
        "Sigma case of discriminant '%a' is not defined in the sigma type."
        EType.pp tdsc
    | IncompatibleSigmaCase tdsc ->
      Fmt.pf ppf "Sigma cases of discriminants '%a' are incompatible." EType.pp
        tdsc
    | MissingSigmaCaseDiscriminant dsc ->
      Fmt.pf ppf "Missing discriminant '%a' from the sigma type case." Id.pp dsc
    | UnknownSigmaCaseDiscriminant tdsc ->
      Fmt.pf ppf "Cannot find discriminant '%a' in the sigma type." EType.pp
        tdsc
    | BadCongruency (tref, tsrc) ->
      Fmt.pf ppf "Value of type '%a' is not congruent with type '%a'." EType.pp
        tsrc EType.pp tref
    | BadSubtyping (tref, tsrc) ->
      Fmt.pf ppf "Value of type '%a' is not assignable to type '%a'." EType.pp
        tsrc EType.pp tref
    | BadOperand (tpx, targ) ->
      Fmt.pf ppf "Argument of type '%a' is not assignable to '%a' operand."
        EType.pp targ EType.pp tpx
    | BadReturn (tret, tsrc) ->
      Fmt.pf ppf "Value of type '%a' cannot be returned by a '%a' function."
        EType.pp tsrc EType.pp tret

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

let update (msg : msg) (err : t) : t =
  match err.msgs with
  | [] -> { err with msgs = [ msg ] }
  | _ :: msgs -> { err with msgs = msg :: msgs }

let src (err : t) : ErrSrc.t = err.src

let set_src (src : ErrSrc.t) (err : t) : t = { err with src }

let pp code (ppf : Format.formatter) (err : t) : unit =
  let module MsgFmt = Error_type.ErrorTypeFmt (TypingErr) in
  let module ErrSrcFmt = ErrSrc.ErrSrcFmt (TypingErr) in
  Fmt.pf ppf "%a%a" MsgFmt.pp err.msgs ErrSrcFmt.pp (code, err.src)

let str code (err : t) = Fmt.str "%a" (pp code) err
