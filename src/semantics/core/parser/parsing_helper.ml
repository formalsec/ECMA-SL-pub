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
 *.
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

open Prelude
open EslBase
open EslSyntax
open EslSyntax.Source

module Func = struct
  let parse_params (pxs : Id.t list) : Id.t list =
    let check_dups checked px =
      if not (Hashtbl.mem checked px.it) then Hashtbl.replace checked px.it ()
      else Compile_error.(throw ~src:px.at (DuplicatedParam px))
    in
    List.iter (check_dups (Hashtbl.create (List.length pxs))) pxs;
    pxs
end

module Stmt = struct
  open Stmt

  let parse_switch_cases (css : (Expr.t * t) list) : (Value.t, t) Hashtbl.t =
    let val_of_expr e =
      match e.it with
      | Expr.Val v -> (v, e.at)
      | _ -> Log.fail "expecting a value expression, but got %a" Expr.pp e
    in
    let check_dups css (e, s) =
      let (v, at) = val_of_expr e in
      if not (Hashtbl.mem css v) then Hashtbl.replace css v s
      else Compile_error.(throw ~src:at (DuplicatedSwitchCase v))
    in
    let parsed_css = Hashtbl.create (List.length css) in
    List.iter (check_dups parsed_css) css;
    parsed_css
end

module Expr = struct
  open Expr

  let parse_return_expr (e : t option) : t =
    Option.value ~default:(Val Value.void @> none) e
end
