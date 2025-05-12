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
open EslSyntax

type tenv = (Id.t', tvar) Hashtbl.t

and tvar =
  { tsig : EType.t option
  ; tref : EType.t'
  }

let tvar_create (tsig : EType.t option) (tref : EType.t) : tvar =
  { tsig; tref = tref.it }

type t =
  { prog : EProg.t
  ; func : EFunc.t
  ; tenv : tenv
  ; tsafe : bool
  }

let default () : t =
  { prog = EProg.default ()
  ; func = EFunc.default ()
  ; tenv = Hashtbl.create !Base.default_hashtbl_sz
  ; tsafe = true
  }

let create (p : EProg.t) : t = { (default ()) with prog = p }

let prog (tctx : t) : EProg.t = tctx.prog

let func (tctx : t) : EFunc.t = tctx.func

let set_func (f : EFunc.t) (tctx : t) : t = { tctx with func = f }

let curr_treturn (tctx : t) : EType.t =
  EFunc.treturn tctx.func |> EType.resolve_topt

let safe_exec code (f : t -> t) (tctx : t) : t =
  try f tctx
  with Typing_error.Error err ->
    Log.stderr "%a@." (Typing_error.pp code) err;
    { tctx with tsafe = false }

let tenv_reset (tctx : t) : unit = Hashtbl.reset tctx.tenv

let tenv_find (tctx : t) (x : Id.t) : tvar option =
  Hashtbl.find_opt tctx.tenv x.it

let tenv_set (tctx : t) (x : Id.t) (tvar : tvar) =
  Hashtbl.replace tctx.tenv x.it tvar
