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
open EslSyntax.Source

module Prog = struct
  open EProg

  let parse_tdef (tdef : EType.TDef.t) (p : t) : unit =
    let tn = EType.TDef.name tdef in
    match Hashtbl.find_opt p.tdefs tn.it with
    | None -> Hashtbl.replace p.tdefs tn.it tdef
    | Some _ -> Compile_error.(throw ~src:tn.at (DuplicatedTDef tn))

  let parse_func (func : EFunc.t) (p : t) : unit =
    let fn = EFunc.name func in
    match Hashtbl.find_opt p.funcs fn.it with
    | None -> Hashtbl.replace p.funcs fn.it func
    | Some _ -> Compile_error.(throw ~src:fn.at (DuplicatedFunc fn))

  let parse_macro (macro : EMacro.t) (p : t) : unit =
    let mn = EMacro.name macro in
    match Hashtbl.find_opt p.macros mn.it with
    | None -> Hashtbl.replace p.macros mn.it macro
    | Some _ -> Compile_error.(throw ~src:mn.at (DuplicatedMacro mn))

  let parse_prog (imports : EImport.t list) (parsers : (t -> unit) list) : t =
    let p = { (default ()) with imports } in
    List.iter (fun el_parser -> el_parser p) parsers;
    p
end

module Func = struct
  let parse_params (tpxs : (Id.t * EType.t option) list) :
    (Id.t * EType.t option) list =
    let check_dups checked (px, _) =
      if not (Hashtbl.mem checked px.it) then Hashtbl.replace checked px.it ()
      else Compile_error.(throw ~src:px.at (DuplicatedParam px))
    in
    List.iter (check_dups (Hashtbl.create (List.length tpxs))) tpxs;
    tpxs
end

module Expr = struct
  open EExpr

  let parse_return_expr (e : t option) : t =
    Option.value ~default:(Val Value.void @> none) e

  let parse_object_fields (flds : (Id.t * t) list) : (Id.t * t) list =
    let check_dups checked (fn, _) =
      if not (Hashtbl.mem checked fn.it) then Hashtbl.replace checked fn.it ()
      else Compile_error.(throw ~src:fn.at (DuplicatedField fn))
    in
    List.iter (check_dups (Hashtbl.create (List.length flds))) flds;
    flds
end

module Type = struct
  open EType

  let parse_tobject (flds : (Id.t * t * tfldstyle) list) : tobject =
    let parse_tobjfld_f tflds (fn, ft, fs) =
      if not (Hashtbl.mem tflds fn.it) then
        Hashtbl.replace tflds fn.it (fn, ft, fs)
      else Compile_error.(throw ~src:fn.at (DuplicatedTField fn))
    in
    let retrieve_smry_field tflds =
      let make_smry (fn, ft, _) = (fn, ft) in
      let smry = Option.map make_smry (Hashtbl.find_opt tflds "*") in
      Hashtbl.remove tflds "*";
      smry
    in
    let tflds = Hashtbl.create (List.length flds) in
    List.iter (parse_tobjfld_f tflds) flds;
    let smry = retrieve_smry_field tflds in
    { kind = ObjLit; flds = tflds; smry }

  let parse_tsigma (dsc : Id.t) (t : t) : t list =
    let open Compile_error in
    let parse_dsc checked ot at =
      match Hashtbl.find_opt ot.flds dsc.it with
      | Some (_, ({ it = LiteralType (LitStrong, lt); _ } as tdsc), _) ->
        if not (Hashtbl.mem checked lt) then Hashtbl.replace checked lt ()
        else throw ~src:tdsc.at (DuplicatedSigmaDiscriminant tdsc)
      | Some (_, t', _) -> throw ~src:t'.at UnexpectedSigmaDiscriminant
      | None -> throw ~src:at (MissingSigmaDiscriminant dsc)
    in
    let parse_case_f checked = function
      | { it = ObjectType ot; at } -> parse_dsc checked ot at
      | t' -> throw ~src:t'.at UnexpectedSigmaCase
    in
    let sigma_cases = function
      | { it = UnionType ts; _ } -> ts
      | { it = ObjectType _; _ } as t -> [ t ]
      | t' -> throw ~src:t'.at UnexpectedSigmaCase
    in
    let ts = sigma_cases t in
    List.iter (parse_case_f (Hashtbl.create (List.length ts))) ts;
    ts
end
