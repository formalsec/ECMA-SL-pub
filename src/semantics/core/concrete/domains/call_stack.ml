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
open EslSyntax.Source

type cursor =
  { f : Func.t
  ; s : Stmt.t
  }

type 'store restore =
  { store : 'store
  ; cont : Stmt.t list
  ; retvar : string
  }

let restore (restore : 'store restore) : 'store * Stmt.t list * string =
  (restore.store, restore.cont, restore.retvar)

type 'store frame =
  | Toplevel of cursor
  | Intermediate of cursor * 'store restore

type 'store t = 'store frame list

let default () : 'store t = [] [@@inline]

let create (f : Func.t) : 'store t = [ Toplevel { f; s = Stmt.default () } ]
[@@inline]

let cursor (frame : 'store frame) : cursor =
  match frame with Toplevel cursor | Intermediate (cursor, _) -> cursor

let frame (stack : 'store t) : 'store frame =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | frame :: _ -> frame

let depth (stack : 'store t) : int = List.length stack [@@inline]

let func (stack : 'store t) : Func.t = (cursor @@ frame stack).f [@@inline]

let stmt (stack : 'store t) : Stmt.t = (cursor @@ frame stack).s [@@inline]

let pop (stack : 'store t) : 'store frame * 'store t =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | frame :: stack' -> (frame, stack')

let push (stack : 'store t) (f : Func.t) (store : 'store) (cont : Stmt.t list)
  (retvar : string) : 'store t =
  let cursor = { f; s = Stmt.default () } in
  let restore = { store; cont; retvar } in
  Intermediate (cursor, restore) :: stack

let update (stack : 'store t) (s : Stmt.t) : 'store t =
  match stack with
  | [] -> Log.fail "expecting non-empty call stack"
  | Toplevel cursor :: stack' -> Toplevel { cursor with s } :: stack'
  | Intermediate (cursor, r) :: stack' ->
    Intermediate ({ cursor with s }, r) :: stack'

let pp_loc (ppf : Format.formatter) (at : at) : unit =
  Fmt.pf ppf "file %S, line %d" at.file at.lpos.line

let pp (ppf : Format.formatter) (stack : 'store t) : unit =
  let pp_bind ppf frame = Fmt.string ppf (Func.name' (cursor frame).f) in
  if depth stack = 0 then Fmt.string ppf "{}"
  else Fmt.pf ppf "{ %a }" Fmt.(list ~sep:comma pp_bind) stack

let str (stack : 'store t) : string = Fmt.str "%a" pp stack [@@inline]
