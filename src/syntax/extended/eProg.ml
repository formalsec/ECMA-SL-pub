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

type t =
  { file : Id.t'
  ; path : Id.t'
  ; imports : EImport.t list
  ; tdefs : (Id.t', EType.TDef.t) Hashtbl.t
  ; macros : (Id.t', EMacro.t) Hashtbl.t
  ; funcs : (Id.t', EFunc.t) Hashtbl.t
  }

let default () : t =
  { file = ""
  ; path = ""
  ; imports = []
  ; tdefs = Hashtbl.create !Base.default_hashtbl_sz
  ; macros = Hashtbl.create !Base.default_hashtbl_sz
  ; funcs = Hashtbl.create !Base.default_hashtbl_sz
  }

let create (file : Id.t') (path : string) (imports : EImport.t list)
  (tdefs : (Id.t', EType.TDef.t) Hashtbl.t) (funcs : (Id.t', EFunc.t) Hashtbl.t)
  (macros : (Id.t', EMacro.t) Hashtbl.t) : t =
  { file; path; imports; tdefs; funcs; macros }
[@@inline]

let file (p : t) : Id.t' = p.file [@@inline]

let path (p : t) : string = p.path [@@inline]

let imports (p : t) : EImport.t list = p.imports [@@inline]

let tdefs (p : t) : (Id.t', EType.TDef.t) Hashtbl.t = p.tdefs [@@inline]

let macros (p : t) : (Id.t', EMacro.t) Hashtbl.t = p.macros [@@inline]

let funcs (p : t) : (Id.t', EFunc.t) Hashtbl.t = p.funcs [@@inline]

let pp (ppf : Format.formatter) (p : t) : unit =
  let newline ppf () = Fmt.pf ppf "@\n" in
  let pp_div ppf len = if len > 0 then Fmt.pf ppf "@\n@\n" else () in
  let pp_bind pp_v ppf (_, v) = pp_v ppf v in
  let pp_list pp_v ppf vs = Fmt.(list ~sep:newline pp_v) ppf vs in
  let pp_tbl pp_v ppf vs = Fmt.(hashtbl ~sep:newline (pp_bind pp_v)) ppf vs in
  let pp_tbl2 pp_v ppf vs =
    Fmt.(hashtbl ~sep:(fun fmt () -> Fmt.pf fmt "@\n@\n") (pp_bind pp_v)) ppf vs
  in
  Fmt.pf ppf "%a%a%a%a%a%a%a" (pp_list EImport.pp) p.imports pp_div
    (List.length p.imports) (pp_tbl EType.TDef.pp) p.tdefs pp_div
    (Hashtbl.length p.tdefs) (pp_tbl2 EMacro.pp) p.macros pp_div
    (Hashtbl.length p.macros) (pp_tbl2 EFunc.pp) p.funcs

let str (p : t) : string = Fmt.str "%a" pp p [@@inline]

let lambdas (p : t) : (at * Id.t' * Id.t list * Id.t list * EStmt.t) list =
  Hashtbl.fold (fun _ f acc -> EFunc.lambdas f @ acc) p.funcs []
