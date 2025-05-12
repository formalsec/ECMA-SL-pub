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
open Source

module Imports = struct
  open EImport

  let load_dependency code (file : Id.t) (path : string) : EProg.t =
    try
      EParsing.(
        load_file code ~file:file.it path |> parse_eprog ~file:file.it path )
    with Not_found ->
      Compile_error.(throw ~src:file.at (UnknownDependency file))

  let set_import_prefix (stdlib : string) (workspace : string)
    (import : EImport.t) : Id.t * string =
    match import.it with
    | User import' -> (import', Filename.concat workspace import'.it)
    | Standard import' -> (import', Filename.concat stdlib (import'.it ^ ".esl"))

  let relativize (file : Id.t') (imports : EImport.t list) : EImport.t list =
    let relativize_f dir import =
      match import.it with
      | User import' ->
        User (Source.map (Filename.concat dir) import') @> import.at
      | Standard _ -> import
    in
    List.map (relativize_f (Filename.dirname file)) imports

  let import_resolver ~(stdlib : string) code (workspace : string) (p : EProg.t)
    (resolved : (Id.t', unit) Hashtbl.t)
    (unresolved : (Id.t' * EImport.t list) list) : unit =
    let rec loop =
      let open Source in
      function
      | [] -> ()
      | (source, []) :: unresolved' ->
        Hashtbl.replace resolved source ();
        loop unresolved'
      | (source, import :: imports') :: unresolved' as unresolved ->
        let (import, dependency_path) =
          set_import_prefix stdlib workspace import
        in
        if Hashtbl.mem resolved import.it then
          loop ((source, imports') :: unresolved')
        else
          let open EParsing_helper.Prog in
          let dependency = load_dependency code import dependency_path in
          let dependency_imports =
            relativize import.it (EProg.imports dependency)
          in
          let new_dependencies = (import.it, dependency_imports) in
          Hashtbl.iter (fun _ t -> parse_tdef t p) (EProg.tdefs dependency);
          Hashtbl.iter (fun _ f -> parse_func f p) (EProg.funcs dependency);
          Hashtbl.iter (fun _ m -> parse_macro m p) (EProg.macros dependency);
          loop (new_dependencies :: unresolved)
    in
    loop unresolved

  let resolve_imports ~(stdlib : string) code (p : EProg.t) : EProg.t =
    let workspace = Filename.dirname (EProg.path p) in
    let resolved = Hashtbl.create !Base.default_hashtbl_sz in
    let relative_imports = relativize (EProg.file p) (EProg.imports p) in
    import_resolver ~stdlib code workspace p resolved
      [ (EProg.file p, relative_imports) ];
    { p with imports = [] }
end

module Macros = struct
  let macro_mapper (p : EProg.t) : EStmt.t -> EStmt.t =
   fun s ->
    match s.it with
    | EStmt.MacroApply (mn, es) -> (
      match Hashtbl.find_opt (EProg.macros p) mn.it with
      | None -> Compile_error.(throw ~src:mn.at (UnknownMacro mn))
      | Some m ->
        let pxs = EMacro.params' m in
        let subst =
          try List.combine pxs es |> List.to_seq |> Hashtbl.of_seq
          with _ ->
            let (npxs, nargs) = (List.length pxs, List.length es) in
            Compile_error.(throw ~src:s.at (BadNArgs (npxs, nargs)))
        in
        EStmt.map ~emapper:(EExpr.Mapper.var subst) EStmt.Mapper.id
          (EMacro.body m) )
    | _ -> s

  let apply_func_macros (p : EProg.t) (f : EFunc.t) : unit =
    let s = EStmt.map (macro_mapper p) f.it.s in
    Hashtbl.replace (EProg.funcs p) (EFunc.name' f)
      { f with it = { f.it with s } }

  let apply_macros (p : EProg.t) : EProg.t =
    Hashtbl.iter (fun _ f -> apply_func_macros p f) p.funcs;
    { p with macros = Hashtbl.create !Base.default_hashtbl_sz }
end
