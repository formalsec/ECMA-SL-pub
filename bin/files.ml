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

open Smtml_prelude.Result
open Fpath

module Parser = struct
  let parse (str : string) (test_f : t -> (bool, [< `Msg of string ]) result) :
    [> `Ok of t | `Error of string ] =
    let fpath = v str in
    match test_f fpath with
    | Ok true -> `Ok fpath
    | Ok false -> `Error (Format.asprintf "File '%s' not found!" str)
    | Error (`Msg err) -> `Error err

  let fix_dir (fpath : [> `Ok of t | `Error of string ]) :
    [> `Ok of t | `Error of string ] =
    match fpath with
    | `Ok fpath' -> (
      match Bos.OS.Dir.exists fpath' with
      | Ok true -> `Ok (to_dir_path fpath')
      | Ok false -> `Ok fpath'
      | Error (`Msg err) -> `Error err )
    | `Error _ as err -> err

  let fpath = ((fun str -> `Ok (v str)), pp)

  let valid_fpath = ((fun str -> parse str Bos.OS.Path.exists |> fix_dir), pp)

  let non_dir_fpath = ((fun str -> parse str Bos.OS.File.exists), pp)

  let dir_fpath = ((fun str -> parse str Bos.OS.Dir.exists), pp)
end

type output =
  [ `None
  | `Fixed of t
  | `Generated of t
  ]

let get_output (output : output) : t option =
  match output with
  | `None -> None
  | `Fixed output' -> Some output'
  | `Generated output' -> Some output'

let dir_contents (recursive : bool) (dir : t) : t list Result.t =
  let fold_f fpath acc = fpath :: acc in
  let traverse = if recursive then `Any else `None in
  Result.bos (Bos.OS.Dir.fold_contents ~elements:`Files ~traverse fold_f [] dir)

let read_inputs (multiple : bool) (recursive : bool) (input : t) :
  (t * t) list Result.t =
  if is_dir_path input then
    let* contents = dir_contents recursive input in
    let workspace = if multiple then parent input else input in
    Ok (List.map (fun fpath' -> (workspace, fpath')) contents)
  else Ok [ (parent input, input) ]

let flat_inputs (inputs : (t * t) list list) : (t * t) list Result.t =
  let inputs' = List.flatten inputs in
  if inputs' != [] then Ok inputs'
  else Result.error (`Generic "Empty input list.")

let make_subdir (dir : t) (workspace : t) (input : t) (outext : string) :
  output Result.t =
  let rel_input = Option.get (relativize ~root:workspace input) in
  let output = (dir // rem_ext rel_input) + outext in
  match Bos.OS.Dir.create (parent output) with
  | Ok _ -> Ok (`Generated output)
  | Error (`Msg err) -> Result.error (`Generic err)

let make_fout (output : t option) (workspace : t) (input : t)
  (outext : string option) : output Result.t =
  let outext = Option.value ~default:(get_ext input) outext in
  match output with
  | Some dir when is_dir_path dir -> make_subdir dir workspace input outext
  | Some output' -> Ok (`Fixed output')
  | None -> Ok `None

let generate_input_list ?(recursive : bool = true) (inputs : t list) :
  (t * t) list Result.t =
  let multiple = List.length inputs > 1 in
  let* inputs' = list_map (read_inputs multiple recursive) inputs in
  flat_inputs inputs'

let process_inputs ?(jobs = 1) ?(outext : string option)
  (exec_f : t -> t -> output -> unit Result.t) (inputs : (t * t) list)
  (output : t option) : unit Result.t =
  let work (workspace, input) () =
    (* TODO: Eio.Time.with_timeout_exn 100.0 @@ fun () -> *)
    let* output' = make_fout output workspace input outext in
    exec_f workspace input output'
  in
  if jobs <= 1 then Ok (List.iter (fun input -> ignore @@ work input ()) inputs)
  else
    Eio_main.run @@ fun env ->
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    Eio.Switch.run @@ fun sw ->
    let pool = Eio.Executor_pool.create ~sw domain_mgr ~domain_count:jobs in
    let results =
      List.map
        (fun input ->
          Eio.Executor_pool.submit_fork ~sw pool ~weight:0.8 (work input) )
        inputs
    in
    List.iter (fun result -> ignore @@ Eio.Promise.await_exn result) results;
    Ok ()
