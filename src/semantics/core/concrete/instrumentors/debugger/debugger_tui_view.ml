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
open Debugger_tui_helper

type t = { frame : Win.t Frame.t }

let create (acs : Acs.acs) (consolewin : Win.t) (codewin : Win.t) : t =
  let (y, x) = (0, codewin.xz - 1) in
  let yz = codewin.yz in
  let xz = consolewin.xz - codewin.xz + 1 in
  let border = Frame.{ (dflt_border acs) with tl = acs.ttee; bl = acs.btee } in
  let framewin = Win.mk consolewin y x yz xz in
  let win = Win.mk framewin 1 1 (yz - 2) (xz - 2) in
  let frame = Frame.mk ~border acs framewin (Win.element win) in
  { frame }

let resize (view : t) (consolewin : Win.t) (codewin : Win.t) : t =
  let (y, x) = (0, codewin.xz - 1) in
  let yz = codewin.yz in
  let xz = consolewin.xz - codewin.xz + 1 in
  let framewin = Win.mk consolewin y x yz xz in
  let win = Win.mk framewin 1 1 (yz - 2) (xz - 2) in
  let frame = Frame.resize view.frame framewin (Win.element win) in
  { frame }

let window (view : t) : window = Frame.window view.frame

let refresh (view : t) : unit = Frame.refresh view.frame

let rec element (view : t) : t element = { v = view; window; refresh; element }

let render_static (view : t) : unit = Frame.draw view.frame
