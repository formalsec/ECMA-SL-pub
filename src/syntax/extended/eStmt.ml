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

open EslBase
open Source

type t = t' Source.t

and t' =
  | Skip
  | Debug of t
  | Block of t list
  | ExprStmt of EExpr.t
  | Print of EExpr.t
  | Return of EExpr.t
  | Assert of EExpr.t
  | Fail of EExpr.t
  | Throw of EExpr.t
  | MacroApply of Id.t * EExpr.t list
  | Assign of Id.t * EType.t option * EExpr.t
  | GAssign of Id.t * EExpr.t
  | FieldAssign of EExpr.t * EExpr.t * EExpr.t
  | FieldDelete of EExpr.t * EExpr.t
  | Lambda of Id.t * string * Id.t list * Id.t list * t
  | If of EExpr.t * t * t option
  | While of EExpr.t * t
  | ForEach of Id.t * EExpr.t * t
  | RepeatUntil of t * EExpr.t option
  | Switch of EExpr.t * (EExpr.t * t) list * t option
  | MatchWith of EExpr.t * Id.t option * (EPat.t * t) list

let default : unit -> t =
  let dlft = Skip @> none in
  fun () -> dlft

let rec pp (ppf : Format.formatter) (s : t) : unit =
  let pp_vs pp_v ppf es = Fmt.(list ~sep:comma pp_v) ppf es in
  let pp_indent pp_v ppf = Fmt.pf ppf "  @[<v>%a@]" pp_v in
  let pp_cont fmt s =
    Fmt.pf fmt (match Source.view s with Block _ -> " " | _ -> "@\n")
  in
  let pp_nested ppf s =
    match s.it with
    | Block _ -> Fmt.pf ppf " %a" pp s
    | _ -> Fmt.pf ppf "@\n%a" (pp_indent pp) s
  in
  let pp_else_if ppf s2 =
    match s2.it with
    | If _ -> Fmt.pf ppf " %a" pp s2
    | _ -> Fmt.pf ppf "%a" pp_nested s2
  in
  let newline ppf () = Fmt.pf ppf "@\n" in
  match s.it with
  | Skip -> Fmt.string ppf ";"
  | Debug s' -> Fmt.pf ppf "# %a" pp s'
  | Block ss ->
    Fmt.pf ppf "{@\n%a@\n}" (pp_indent Fmt.(list ~sep:newline pp)) ss
  | ExprStmt e -> Fmt.pf ppf "%a;" EExpr.pp e
  | Print e -> Fmt.pf ppf "print %a;" EExpr.pp e
  | Return e ->
    if EExpr.isvoid e then Fmt.string ppf "return;"
    else Fmt.pf ppf "return %a;" EExpr.pp e
  | Assert e -> Fmt.pf ppf "assert %a;" EExpr.pp e
  | Fail e -> Fmt.pf ppf "fail %a;" EExpr.pp e
  | Throw e -> Fmt.pf ppf "throw %a;" EExpr.pp e
  | MacroApply (mn, es) -> Fmt.pf ppf "@%a(%a);" Id.pp mn (pp_vs EExpr.pp) es
  | Assign (x, t, e) ->
    Fmt.pf ppf "%a%a := %a;" Id.pp x EType.tannot_pp t EExpr.pp e
  | GAssign (x, e) -> Fmt.pf ppf "|%a| := %a;" Id.pp x EExpr.pp e
  | FieldAssign (oe, fe, e) ->
    Fmt.pf ppf "%a%a := %a;" EExpr.pp oe EExpr.pp_lookup fe EExpr.pp e
  | FieldDelete (oe, fe) ->
    Fmt.pf ppf "delete %a%a;" EExpr.pp oe EExpr.pp_lookup fe
  | If (e, s1, s2) ->
    let pp_else ppf s2 = Fmt.pf ppf "%aelse%a" pp_cont s1 pp_else_if s2 in
    Fmt.pf ppf "if (%a)%a%a" EExpr.pp e pp_nested s1 (Fmt.option pp_else) s2
  | While (e, s') -> Fmt.pf ppf "while (%a)%a" EExpr.pp e pp_nested s'
  | ForEach (x, e, s') ->
    Fmt.pf ppf "foreach (%a : %a)%a" Id.pp x EExpr.pp e pp_nested s'
  | RepeatUntil (s', e) ->
    let pp_until ppf e = Fmt.pf ppf "%auntil (%a);" pp_cont s EExpr.pp e in
    Fmt.pf ppf "repeat %a%a" pp_nested s' (Fmt.option pp_until) e
  | Switch (e, css, dflt) ->
    let pp_dflt_cs ppf s = Fmt.pf ppf "default:%a" pp_nested s in
    let pp_dflt ppf s = (Fmt.option pp_dflt_cs) ppf s in
    let pp_cs ppf (v, s) = Fmt.pf ppf "case %a:%a" EExpr.pp v pp_nested s in
    let pp_css ppf css = Fmt.(list ~sep:newline pp_cs) ppf css in
    let pp' ppf (css, dflt) = Fmt.pf ppf "%a%a" pp_css css pp_dflt dflt in
    Fmt.pf ppf "switch (%a) {@\n%a@\n}" EExpr.pp e (pp_indent pp') (css, dflt)
  | MatchWith (e, dsc, css) ->
    let pp_dsc_v ppf dsc = Fmt.pf ppf " : %a" Id.pp dsc in
    let pp_dsc ppf dsc = Fmt.option pp_dsc_v ppf dsc in
    let pp_cs ppf (pat, s) = Fmt.pf ppf "| %a ->%a" EPat.pp pat pp_nested s in
    let pp_css ppf css = Fmt.(list ~sep:newline pp_cs) ppf css in
    Fmt.pf ppf "match %a%a with@\n%a" EExpr.pp e pp_dsc dsc pp_css css
  | Lambda (x, _, pxs, ctxvars, s') ->
    Fmt.pf ppf "%a := lambda (%a) [%a] %a;" Id.pp x (pp_vs Id.pp) pxs
      (pp_vs Id.pp) ctxvars pp s'

let str (s : t) : string = Fmt.str "%a" pp s [@@inline]

let rec map ?(emapper : EExpr.t -> EExpr.t = EExpr.Mapper.id) (mapper : t -> t)
  (s : t) : t =
  let map' s' = map ~emapper mapper s' in
  let mapper' s' = mapper (s' @> s.at) in
  let id_mapper (x : Id.t) =
    match (emapper (EExpr.Var x.it @> none)).it with
    | EExpr.Var y -> y @> x.at
    | e -> Log.fail "expecting var in LHS, but got %a" EExpr.pp (e @> none)
  in
  mapper'
  @@
  match s.it with
  | Skip -> Skip
  | Debug s' -> Debug (map' s')
  | Block ss -> Block (List.map map' ss)
  | ExprStmt e -> ExprStmt (emapper e)
  | Print e -> Print (emapper e)
  | Return e -> Return (emapper e)
  | Assert e -> Assert (emapper e)
  | Fail e -> Fail (emapper e)
  | Throw e -> Throw (emapper e)
  | MacroApply (mn, es) -> MacroApply (mn, List.map emapper es)
  | Assign (x, t, e) -> Assign (id_mapper x, t, emapper e)
  | GAssign (x, e) -> GAssign (id_mapper x, emapper e)
  | FieldAssign (oe, fe, e) -> FieldAssign (emapper oe, emapper fe, emapper e)
  | FieldDelete (oe, fe) -> FieldDelete (emapper oe, emapper fe)
  | Lambda (x, id, pxs, ctxvars, s') -> Lambda (x, id, pxs, ctxvars, map' s')
  | If (e, s1, s2) -> If (emapper e, map' s1, Option.map map' s2)
  | While (e, s') -> While (emapper e, map' s')
  | ForEach (x, e, s') -> ForEach (id_mapper x, emapper e, map' s')
  | RepeatUntil (s', e) -> RepeatUntil (map' s', Option.map emapper e)
  | Switch (e, css, dflt) ->
    let map_cs (e, s) = (emapper e, map' s) in
    Switch (emapper e, List.map map_cs css, Option.map map' dflt)
  | MatchWith (e, dsc, css) ->
    let map_cs (pat, s) = (pat, map' s) in
    MatchWith (emapper e, Option.map id_mapper dsc, List.map map_cs css)

let rec to_list ?(recursive : bool = false) (to_list_f : t -> 'a list) (s : t) :
  'a list =
  let to_list_s s = to_list ~recursive to_list_f s in
  let to_list_ss stmts = List.concat (List.map to_list_s stmts) in
  let to_list_opt s = Option.fold ~none:[] ~some:(fun s -> [ s ]) s in
  let to_list_recursive () =
    match s.it with
    | Skip -> []
    | Debug s' -> to_list_s s'
    | Block ss -> to_list_ss ss
    | ExprStmt _ -> []
    | Print _ | Return _ -> []
    | Assert _ | Fail _ | Throw _ -> []
    | MacroApply _ -> []
    | Assign _ | GAssign _ -> []
    | FieldAssign _ | FieldDelete _ -> []
    | If (_, s1, s2) -> to_list_ss (s1 :: to_list_opt s2)
    | While (_, s') -> to_list_s s'
    | ForEach (_, _, s') -> to_list_s s'
    | RepeatUntil (s', _) -> to_list_s s'
    | Switch (_, css, dflt) -> to_list_ss (List.map snd css @ to_list_opt dflt)
    | MatchWith (_, _, css) -> to_list_ss (List.map snd css)
    | Lambda (_, _, _, _, s') -> to_list_s s'
  in
  to_list_f s @ if not recursive then [] else to_list_recursive ()

module Mapper = struct
  let id : t -> t = Fun.id
end
