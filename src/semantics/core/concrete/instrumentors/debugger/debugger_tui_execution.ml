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
  let (y, x) = (codewin.yz - 1, 0) in
  let yz = consolewin.yz - codewin.yz + 1 in
  let xz = proportional_sz consolewin.xz 5 2 in
  let border = Frame.{ (dflt_border acs) with tl = acs.ltee; tr = acs.ttee } in
  let framewin = Win.mk consolewin y x yz xz in
  let win = Win.mk framewin 1 1 (yz - 2) (xz - 2) in
  let frame = Frame.mk ~border acs framewin (Win.element win) in
  { frame }

let resize (exec : t) (consolewin : Win.t) (codewin : Win.t) : t =
  let (y, x) = (codewin.yz - 1, 0) in
  let yz = consolewin.yz - codewin.yz + 1 in
  let xz = proportional_sz consolewin.xz 5 2 in
  let framewin = Win.mk consolewin y x yz xz in
  let win = Win.mk framewin 1 1 (yz - 2) (xz - 2) in
  let frame = Frame.resize exec.frame framewin (Win.element win) in
  { frame }

let window (exec : t) : window = Frame.window exec.frame

let refresh (exec : t) : unit = Frame.refresh exec.frame

let rec element (exec : t) : t element = { v = exec; window; refresh; element }

let render_static (exec : t) : unit = Frame.draw exec.frame
