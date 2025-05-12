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
open EslSyntax

let ( let* ) = Result.bind

type obj = Value.t Object.t

type store = Value.t Store.t

type heap = Value.t Heap.t

type stack = Value.t Store.t Call_stack.t

module IEntry = struct
  type t =
    { main : string
    ; heap : heap option
    }

  let default () : t = { main = "main"; heap = None }
end

module IResult = struct
  type t =
    { retval : Value.t
    ; heap : heap
    ; metrics : Yojson.Basic.t
    }

  let unwrap_opt obj prop =
    match Object.get obj prop with
    | Some v -> Ok v
    | None -> Error (`Execute (Fmt.str "No value for property \"%s\"" prop))

  let get_completion (heap : heap) loc =
    match Heap.get heap loc with
    | None -> Result.error (`Execute "Leaked invalid location")
    | Some obj -> (
      match Object.get obj "__completion__" with
      | None -> Result.error (`Execute "Object is not a completion")
      | Some _ ->
        let* type_ = unwrap_opt obj "type" in
        let* value = unwrap_opt obj "value" in
        let* target = unwrap_opt obj "target" in
        Ok (type_, value, target) )

  let is_normal_completion (heap : heap) (loc : int) =
    let open Smtml_prelude.Result in
    let+ (type_, _, _) = get_completion heap loc in
    match type_ with Str "normal" -> true | _ -> false
end

module IConst = struct
  let global_loc : Loc.t = 0
end

module IConfig = struct
  let print_depth : int option ref = ref None

  let resolve_exitval : bool ref = ref true

  let show_exitval : bool ref = ref false
end
