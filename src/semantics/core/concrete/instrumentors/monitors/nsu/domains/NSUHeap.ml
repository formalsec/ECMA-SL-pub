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

type 'sl t = (Loc.t, 'sl NSUObject.t * 'sl * 'sl) Hashtbl.t

let create () : 'sl t = Hashtbl.create !Base.default_hashtbl_sz

let insert (heap : 'sl t) (loc : Loc.t) (obj : 'sl NSUObject.t)
  (struct_lvl : 'sl) (obj_lvl : 'sl) : unit =
  if Hashtbl.mem heap loc then
    raise (NSUException.Exists "Location already exists")
  else Hashtbl.replace heap loc (obj, struct_lvl, obj_lvl)

let delete (heap : 'sl t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : 'sl t) (loc : Loc.t) (obj : 'sl NSUObject.t)
  (struct_lvl : 'sl) (obj_lvl : 'sl) : unit =
  Hashtbl.replace heap loc (obj, struct_lvl, obj_lvl)

let create_object (heap : 'sl t) (loc : Loc.t) (struct_lvl : 'sl) (obj_lvl : 'sl)
  : unit =
  let obj = NSUObject.create () in
  insert heap loc obj struct_lvl obj_lvl

let get (heap : 'sl t) (loc : Loc.t) : 'sl NSUObject.t * 'sl * 'sl =
  match Hashtbl.find_opt heap loc with
  | None -> raise (NSUException.Exists "Cannot find object")
  | Some (obj, struct_lvl, obj_lvl) -> (obj, struct_lvl, obj_lvl)

let get_obj (heap : 'sl t) (loc : Loc.t) : 'sl NSUObject.t option =
  match Hashtbl.find_opt heap loc with
  | None -> None
  | Some (obj, _, _) -> Some obj

let get_struct_lvl (heap : 'sl t) (loc : Loc.t) : 'sl option =
  match Hashtbl.find_opt heap loc with
  | Some (_, struct_lvl, _) -> Some struct_lvl
  | None -> None

let get_obj_lvl (heap : 'sl t) (loc : Loc.t) : 'sl option =
  match Hashtbl.find_opt heap loc with
  | Some (_, _, object_lvl) -> Some object_lvl
  | None -> None

let get_field (heap : 'sl t) (loc : Loc.t) (fn : string) : ('sl * 'sl) option =
  Option.bind (get_obj heap loc) (fun o -> NSUObject.get o fn)

let delete_field (heap : 'sl t) (loc : Loc.t) (fn : string) : bool =
  let delete_field' obj = NSUObject.delete obj fn |> fun () -> true in
  get_obj heap loc |> Option.fold ~none:false ~some:delete_field'

let new_sec_prop (heap : 'sl t) (loc : Loc.t) (fn : string) (exists_lvl : 'sl)
  (val_lvl : 'sl) : bool =
  let new_sec_prop obj =
    NSUObject.new_sec_prop obj fn exists_lvl val_lvl |> fun () -> true
  in
  get_obj heap loc |> Option.fold ~none:false ~some:new_sec_prop

let upg_prop_exists_lvl (heap : 'sl t) (loc : Loc.t) (fn : string) (lvl : 'sl) :
  unit =
  let (obj, _, _) = get heap loc in
  NSUObject.upg_exists obj fn lvl

let upg_prop_val_lvl (heap : 'sl t) (loc : Loc.t) (fn : string) (lvl : 'sl) :
  unit =
  let (obj, _, _) = get heap loc in
  NSUObject.upg_val obj fn lvl

let upg_struct_lvl (heap : 'sl t) (loc : Loc.t) (lvl : 'sl) : unit =
  let (obj, _, obj_lvl) = get heap loc in
  update heap loc obj lvl obj_lvl

let upg_obj_lvl (heap : 'sl t) (loc : Loc.t) (lvl : 'sl) : unit =
  let (obj, struct_lvl, _) = get heap loc in
  update heap loc obj struct_lvl lvl

let str (sl_printer : 'sl -> string) (heap : 'sl t) : string =
  let binding_str loc obj struct_lvl obj_lvl =
    Fmt.str "%a|-> {%s}_{%s, %s}" Loc.pp loc
      (NSUObject.str sl_printer obj)
      (sl_printer struct_lvl) (sl_printer obj_lvl)
  in
  let heap_str_f loc (obj, struct_lvl, obj_lvl) acc =
    binding_str loc obj struct_lvl obj_lvl :: acc
  in
  Hashtbl.fold heap_str_f heap [] |> String.concat "\n"
