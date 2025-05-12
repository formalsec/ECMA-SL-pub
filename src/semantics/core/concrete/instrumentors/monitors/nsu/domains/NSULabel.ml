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

open EslSyntax

type 'sl t =
  | EmptyLab
  | MergeLab
  | PrintLab of Expr.t
  | ReturnLab of Expr.t
  | AssignLab of (string * Expr.t)
  | AssignCallLab of (string * string * string list * Expr.t list)
  | AssignInObjCheckLab of (string * string * Loc.t * Expr.t * Expr.t)
  | NewLab of (string * Loc.t)
  | FieldAssignLab of (Loc.t * string * Expr.t * Expr.t * Expr.t)
  | FieldLookupLab of (string * Loc.t * string * Expr.t * Expr.t)
  | FieldDeleteLab of (Loc.t * string * Expr.t * Expr.t)
  | BranchLab of (Expr.t * Stmt.t)
  (* Direct Security Level Upgrades *)
  | UpgVarLab of (string * 'sl)
  | UpgPropValLab of (Loc.t * string * Expr.t * Expr.t * 'sl)
  | UpgPropExistsLab of (Loc.t * string * Expr.t * Expr.t * 'sl)
  | UpgObjectLab of (Loc.t * Expr.t * 'sl)
  | UpgStructLab of (Loc.t * Expr.t * 'sl)
  (**)
  | SetTopLab of string
  | AllowFlowLab of string * string

let str (_sl_printer : 'sl -> string) (label : 'sl t) : string =
  match label with
  | EmptyLab -> "EmptyLab"
  | MergeLab -> "MergeLab"
  | PrintLab e -> Printf.sprintf "PrintLab (%s)" (Expr.str e)
  | ReturnLab e -> Printf.sprintf "RetLab (%s)" (Expr.str e)
  | AssignLab (x, e) -> Printf.sprintf "AssignLab (%s, %s)" x (Expr.str e)
  | AssignCallLab (x, _f, _params, es) ->
    Printf.sprintf "AssignCallLab(%s, [%s])" x
      (List.map Expr.str es |> String.concat ";")
  | AssignInObjCheckLab _ -> "AssignInObjCheckLab"
  | NewLab _ -> "FIXME: NewLab.str"
  | FieldAssignLab _ -> "FIXME: FieldAssignLab.str"
  | FieldLookupLab _ -> "FIXME: FieldLookupLab.str"
  | FieldDeleteLab _ -> "FIXME: FieldDeleteLab.str"
  | BranchLab (expr, stmt) ->
    Printf.sprintf "BranchLab (%s){%s}" (Expr.str expr) (Stmt.str stmt)
  | UpgVarLab _ -> "UpgVarLab"
  | UpgPropValLab _ -> "UpgPropLab"
  | UpgPropExistsLab _ -> "UpgPropLab"
  | UpgObjectLab _ -> "UpgStructLab"
  | UpgStructLab _ -> "UpgStructLab"
  | SetTopLab st -> Printf.sprintf "TopLevelLab (%s)" st
  | AllowFlowLab (st1, st2) -> Printf.sprintf "AllowFlowLab (%s, %s)" st1 st2

let interceptor (parse_sl : string -> 'sl) (func : string) (vs : Value.t list)
  (es : Expr.t list) : 'sl t option =
  let open Source in
  match (func, vs, es) with
  | ( "upgVar"
    , [ Value.Str x; Value.Str lvl_str ]
    , [ { it = Expr.Val (Str x'); _ }; { it = Expr.Val (Str lvl_str'); _ } ] )
    when x = x' && lvl_str = lvl_str' ->
    Some (UpgVarLab (x, parse_sl lvl_str))
  | ( "upgPropExists"
    , [ App (`Op "loc", [ Int loc ]); Value.Str x; Value.Str lvl_str ]
    , [ e_o; e_f; { it = Expr.Val (Str lvl_str'); _ } ] )
    when lvl_str = lvl_str' ->
    Some (UpgPropExistsLab (loc, x, e_o, e_f, parse_sl lvl_str))
  | ( "upgPropExists"
    , [ App (`Op "loc", [ Int _loc ]); Value.Str _x; Value.Str _lvl_str ]
    , [ _; _; _ ] ) ->
    raise (NSUException.Except "Level is not a literal")
  | ( "upgPropVal"
    , [ App (`Op "loc", [ Int loc ]); Value.Str x; Value.Str lvl_str ]
    , [ e_o; e_f; { it = Expr.Val (Str lvl_str'); _ } ] )
    when lvl_str = lvl_str' ->
    Some (UpgPropValLab (loc, x, e_o, e_f, parse_sl lvl_str))
  | ( "upgPropVal"
    , [ App (`Op "loc", [ Int _ ]); Value.Str _x; Value.Str _lvl_str ]
    , [ _; _; _ ] ) ->
    raise (NSUException.Except "Level is not a literal")
  | ( "upgStruct"
    , [ App (`Op "loc", [ Int loc ]); Value.Str lvl_str ]
    , [ e_o; { it = Expr.Val (Str lvl_str'); _ } ] )
    when lvl_str = lvl_str' ->
    Some (UpgStructLab (loc, e_o, parse_sl lvl_str))
  | ( "upgStruct"
    , [ App (`Op "loc", [ Int _loc ]); Value.Str _lvl_str ]
    , [ _e_o; _ ] ) ->
    raise (NSUException.Except "Level is not a literal")
  | ( "upgObject"
    , [ App (`Op "loc", [ Int loc ]); Value.Str lvl_str ]
    , [ e_o; { it = Expr.Val (Str lvl_str'); _ } ] )
    when lvl_str = lvl_str' ->
    Some (UpgObjectLab (loc, e_o, parse_sl lvl_str))
  | ( "upgObject"
    , [ App (`Op "loc", [ Int _loc ]); Value.Str _lvl_str ]
    , [ _e_o; _ ] ) ->
    raise (NSUException.Except "Level is not a literal ")
  | ("setTop", [ Value.Str str ], [ { it = Expr.Val (Str str'); _ } ])
    when str = str' ->
    Some (SetTopLab str)
  | ("setTop", [ Value.Str _str ], [ _ ]) ->
    raise (NSUException.Except "Level is not a string")
  | ( "allowFlow"
    , [ Value.Str str1; Value.Str str2 ]
    , [ { it = Expr.Val (Str str1'); _ }; { it = Expr.Val (Str str2'); _ } ] )
    when str1 = str1' && str2 = str2' ->
    Some (AllowFlowLab (str1, str2))
  | ("allowFlow", [ _; _ ], [ _; _ ]) ->
    raise (NSUException.Except "Level is not a string")
  | _ -> None
