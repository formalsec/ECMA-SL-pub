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

type t =
  [ `Abort of string
  | `Assert_failure of EslSyntax.Stmt.t * Smtml.Expr.t
  | `Eval_failure of Smtml.Expr.t
  | `Exec_failure of Smtml.Expr.t
  | `ReadFile_failure of Smtml.Expr.t
  | `Failure of string
  ]

let pp fmt = function
  | `Abort msg -> Fmt.pf fmt "Abort: %s" msg
  | `Assert_failure (stmt, v) ->
    let pos = stmt.EslSyntax.Source.at in
    Fmt.pf fmt
      "%a: Assert failure:@\n@[<hov 1> Stmt:@;%a@]@\n@[<hov 1> Expr:@;%a@]"
      EslSyntax.Source.pp_at pos EslSyntax.Stmt.pp stmt Smtml.Expr.pp v
  | `Eval_failure v -> Fmt.pf fmt "Eval failure: %a" Smtml.Expr.pp v
  | `Exec_failure v -> Fmt.pf fmt "Exec failure: %a" Smtml.Expr.pp v
  | `ReadFile_failure v -> Fmt.pf fmt "ReadFile failure: %a" Smtml.Expr.pp v
  | `Failure msg -> Fmt.pf fmt "Failure: %s" msg

let to_json = function
  | `Abort msg -> `Assoc [ ("type", `String "Abort"); ("sink", `String msg) ]
  | `Assert_failure (_, v) ->
    let v = Smtml.Expr.to_string v in
    `Assoc [ ("type", `String "Assert failure"); ("sink", `String v) ]
  | `Eval_failure v ->
    let v = Smtml.Expr.to_string v in
    `Assoc [ ("type", `String "Eval failure"); ("sink", `String v) ]
  | `Exec_failure v ->
    let v = Smtml.Expr.to_string v in
    `Assoc [ ("type", `String "Exec failure"); ("sink", `String v) ]
  | `ReadFile_failure v ->
    let v = Smtml.Expr.to_string v in
    `Assoc [ ("type", `String "ReadFile failure"); ("sink", `String v) ]
  | `Failure msg ->
    `Assoc [ ("type", `String "Failure"); ("sink", `String msg) ]
