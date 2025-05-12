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

open Source

type t = t' Source.t

and t' =
  | Skip
  | Merge
  | Debug of t
  | Block of t list
  | Print of Expr.t
  | Return of Expr.t
  | Assign of Id.t * Expr.t
  | AssignCall of Id.t * Expr.t * Expr.t list
  | AssignECall of Id.t * Id.t * Expr.t list
  | AssignNewObj of Id.t
  | AssignObjToList of Id.t * Expr.t
  | AssignObjFields of Id.t * Expr.t
  | AssignInObjCheck of Id.t * Expr.t * Expr.t
  | FieldLookup of Id.t * Expr.t * Expr.t
  | FieldAssign of Expr.t * Expr.t * Expr.t
  | FieldDelete of Expr.t * Expr.t
  | If of Expr.t * t * t option
  | While of Expr.t * t
  | Switch of Expr.t * (Value.t, t) Hashtbl.t * t option
  | Fail of Expr.t
  | Assert of Expr.t

let default : unit -> t =
  let dlft = Skip @> none in
  fun () -> dlft

let view stmt = Source.view stmt

let rec pp (ppf : Format.formatter) (s : t) : unit =
  let pp_vs pp_v ppf es = Fmt.(list ~sep:comma pp_v) ppf es in
  let pp_indent pp_v ppf = Fmt.pf ppf "@\n  @[<v>%a@]@\n" pp_v in
  let newline ppf () = Fmt.pf ppf "@\n" in
  match s.it with
  | Skip -> ()
  | Merge -> ()
  | Debug s' -> Fmt.pf ppf "# %a" pp s'
  | Block ss ->
    Fmt.pf ppf "{%a}"
      (pp_indent Fmt.(list ~sep:(fun fmt () -> Fmt.pf fmt ";@;") pp))
      ss
  | Print e -> Fmt.pf ppf "print %a" Expr.pp e
  | Return e ->
    if Expr.isvoid e then Fmt.string ppf "return"
    else Fmt.pf ppf "return %a" Expr.pp e
  | Fail e -> Fmt.pf ppf "fail %a" Expr.pp e
  | Assert e -> Fmt.pf ppf "assert %a" Expr.pp e
  | Assign (x, e) -> Fmt.pf ppf "%a := %a" Id.pp x Expr.pp e
  | AssignCall (x, fe, es) ->
    Fmt.pf ppf "@[<h>%a := %a(%a)@]" Id.pp x Expr.pp fe (pp_vs Expr.pp) es
  | AssignECall (x, fn, es) ->
    Fmt.pf ppf "@[<h>%a := extern %a(%a)@]" Id.pp x Id.pp fn (pp_vs Expr.pp) es
  | AssignNewObj x -> Fmt.pf ppf "%a := {}" Id.pp x
  | AssignObjToList (x, e) ->
    Fmt.pf ppf "%a := obj_to_list %a" Id.pp x Expr.pp e
  | AssignObjFields (x, e) -> Fmt.pf ppf "%a := obj_fields %a" Id.pp x Expr.pp e
  | AssignInObjCheck (x, e1, e2) ->
    Fmt.pf ppf "%a := %a in_obj %a" Id.pp x Expr.pp e1 Expr.pp e2
  | FieldLookup (x, oe, fe) ->
    Fmt.pf ppf "%a := %a[%a]" Id.pp x Expr.pp oe Expr.pp fe
  | FieldAssign (oe, fe, e) ->
    Fmt.pf ppf "%a[%a] := %a" Expr.pp oe Expr.pp fe Expr.pp e
  | FieldDelete (oe, fe) -> Fmt.pf ppf "delete %a[%a]" Expr.pp oe Expr.pp fe
  | If (e, s1, s2) ->
    let pp_else ppf s = Fmt.pf ppf " else %a" pp s in
    Fmt.pf ppf "if (%a) %a%a" Expr.pp e pp s1 (Fmt.option pp_else) s2
  | While (e, s) -> Fmt.pf ppf "while (%a) %a" Expr.pp e pp s
  | Switch (e, css, dflt) ->
    let pp_dflt_cs ppf s = Fmt.pf ppf "@\ndefault: %a" pp s in
    let pp_dflt ppf s = (Fmt.option pp_dflt_cs) ppf s in
    let pp_cs ppf (v, s) = Fmt.pf ppf "case %a: %a" Value.pp v pp s in
    let pp_css ppf css = Fmt.(hashtbl ~sep:newline pp_cs) ppf css in
    let pp ppf (css, dflt) = Fmt.pf ppf "%a%a" pp_css css pp_dflt dflt in
    Fmt.pf ppf "switch (%a) {%a}" Expr.pp e (pp_indent pp) (css, dflt)

let pp_simple (ppf : Format.formatter) (s : t) : unit =
  match s.it with
  | Block _ -> Fmt.pf ppf "{ ... }"
  | If (e, _, _) -> Fmt.pf ppf "if (%a) { ..." Expr.pp e
  | While (e, _) -> Fmt.pf ppf "while (%a) { ..." Expr.pp e
  | Switch (e, _, _) -> Fmt.pf ppf "switch (%a) { ..." Expr.pp e
  | _ -> pp ppf s

let str (s : t) : string = Fmt.str "%a" pp s [@@inline]
