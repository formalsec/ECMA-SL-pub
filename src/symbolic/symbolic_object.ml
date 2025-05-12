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

let eq v1 v2 = Smtml.Expr.relop Ty_bool Eq v1 v2

let ite c v1 v2 = Smtml.Expr.Bool.ite c v1 v2

let undef = Symbolic_value.mk_symbol "undefined"

module VMap = Map.Make (Symbolic_value)

type value = Symbolic_value.value

type t =
  { fields : value VMap.t
  ; symbols : value VMap.t
  }

let create () = { fields = VMap.empty; symbols = VMap.empty }

let clone { fields; symbols } =
  (* Immutable structures don't need to be copied *)
  { fields; symbols }

let is_empty o = VMap.(is_empty o.fields && is_empty o.symbols)

let to_list o = VMap.bindings o.symbols @ VMap.bindings o.fields

let get_fields o =
  let symbols = VMap.fold (fun key _ acc -> key :: acc) o.symbols [] in
  VMap.fold (fun key _ acc -> key :: acc) o.fields symbols

let has_field o k =
  if VMap.is_empty o.fields && VMap.is_empty o.symbols then
    Symbolic_value.Bool.const false
  else
    match Smtml.Expr.view k with
    | Val _ -> Symbolic_value.Bool.const (VMap.mem k o.fields)
    | _ ->
      let r0 =
        VMap.fold
          (fun key _ acc -> ite (eq k key) (Symbolic_value.Bool.const true) acc)
          o.symbols
          (Symbolic_value.Bool.const false)
      in
      VMap.fold
        (fun key _ acc -> ite (eq k key) (Symbolic_value.Bool.const true) acc)
        o.fields r0

let map_ite (m : value VMap.t) ~(key : value) ~(data : value) =
  VMap.mapi
    (fun key0 data0 ->
      if Symbolic_value.equal key key0 then data0
      else ite (eq key key0) data data0 )
    m

let set o ~key ~data =
  match Smtml.Expr.view key with
  | Val _ -> { o with fields = VMap.add key data o.fields }
  | _ ->
    { fields = map_ite o.fields ~key ~data
    ; symbols = map_ite o.symbols ~key ~data |> VMap.add key data
    }

let fold_eq (m : value VMap.t) (key0 : value) : (value * value) list =
  VMap.fold
    (fun key data acc ->
      if Symbolic_value.equal key0 key then acc else (data, eq key0 key) :: acc )
    m []

(* FIXME: @174 *)
let get { fields; symbols } key =
  match Smtml.Expr.view key with
  | Val _ -> (
    match VMap.find_opt key fields with
    | Some v -> [ (v, []) ]
    | None -> (
      match fold_eq symbols key with
      | [] -> []
      | [ (v, cond) ] ->
        [ (v, [ cond ]); (undef, [ Symbolic_value.Bool.not_ cond ]) ]
      | (v0, cond0) :: tl ->
        let (v, neg_conds) =
          List.fold_left
            (fun (acc, neg_conds) (v1, cond1) ->
              (ite cond1 v1 acc, Symbolic_value.Bool.not_ cond1 :: neg_conds) )
            (v0, [ Symbolic_value.Bool.not_ cond0 ])
            tl
        in
        [ (v, []); (undef, neg_conds) ] ) )
  | _ -> (
    match VMap.find_opt key symbols with
    | Some v -> [ (v, []) ]
    | None -> (
      match fold_eq fields key with
      | [] -> []
      | [ (v, cond) ] ->
        [ (v, [ cond ]); (undef, [ Symbolic_value.Bool.not_ cond ]) ]
      | (v0, cond0) :: tl ->
        let (v, neg_conds) =
          List.fold_left
            (fun (acc, neg_conds) (v1, cond1) ->
              (ite cond1 v1 acc, Symbolic_value.Bool.not_ cond1 :: neg_conds) )
            (v0, [ Symbolic_value.Bool.not_ cond0 ])
            tl
        in
        [ (v, []); (undef, neg_conds) ] ) )

let delete o key =
  match Smtml.Expr.view key with
  | Val _ -> { o with fields = VMap.remove key o.fields }
  | _ ->
    (* Leak *)
    Logs.warn (fun k -> k "Leaking object %a@." Smtml.Expr.pp key);
    o

let pp_map ppf v =
  let map_iter f m = VMap.iter (fun k d -> f (k, d)) m in
  Fmt.iter ~sep:Fmt.comma map_iter
    (fun ppf (key, data) ->
      Fmt.pf ppf {|%a: %a|} Symbolic_value.pp key Symbolic_value.pp data )
    ppf v

let pp ppf { fields; symbols } =
  Fmt.pf ppf "@[<hov 2>{ %a }@]"
    (Fmt.pair ~sep:Fmt.comma pp_map pp_map)
    (fields, symbols)

let to_string o = Fmt.str "%a" pp o

let to_json = to_string
