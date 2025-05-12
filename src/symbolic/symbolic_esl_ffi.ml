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

open Ecma_sl
module Env = Symbolic.Env
module SMap = Link_env.SMap

let of_array arr =
  Array.fold_left (fun m (k, v) -> SMap.add k v m) SMap.empty arr

module type S = sig
  type extern_func

  val extern_cmds : Env.t -> extern_func SMap.t

  val symbolic_api : Fpath.t -> extern_func SMap.t

  val concrete_api : extern_func SMap.t
end

module Make () = struct
  module Choice = Choice_monad.Seq
  module Thread = Choice_monad.Thread
  module Extern_func = Symbolic.Extern_func
  module Optimizer = Choice_monad.Optimizer
  open Extern_func

  let g_replace_re = Re.regexp "/\\([^/]+\\)/g"

  let allow_lazy_values = ref true

  (* FIXME: This table should be part of Env.t or Thread.t to allow symbolic execution to branch/emulate the filesystem correctly *)
  let channel_table = Channel_utils.make ()

  let fresh_i = Base.make_name_generator "i"

  let fresh_x = Base.make_name_generator "x"

  let fresh_func = Base.make_name_generator "eval_func_"

  let ok v = Choice.return (Ok v)

  let error v = Choice.return (Error v)

  let ok_v v = ok @@ Smtml.Expr.value v

  let failure fmt = Fmt.kstr (fun s -> error @@ `Failure s) fmt

  let extern_cmds env =
    let parseJS data =
      let open Result in
      let open EslJSParser.Api in
      let result =
        let data =
          match Smtml.Expr.view data with
          | Val (Str data) -> data
          | _ -> assert false
        in
        let* input_file = Bos.OS.File.tmp "__parse_in_%s__.js" in
        let input_file = Fpath.to_string input_file in
        let* output_file = Bos.OS.File.tmp "__parse_out_%s__.js" in
        let output_file = Fpath.to_string output_file in
        Io.write_file input_file data;
        let fid = fresh_func () in
        let* () =
          Bos.OS.Cmd.run (cmd input_file (Some output_file) (Some fid))
        in
        let data = Io.read_file output_file in
        let func = Parsing.parse_func data in
        Env.add_func env fid func;
        Ok (Smtml.Expr.value (Str fid))
      in
      Choice.return
      @@
      match result with
      | Ok _ as v -> v
      | Error (`Msg err) -> Error (`Failure err)
    in
    of_array [| ("parseJS", Extern_func (Func (Arg Res), parseJS)) |]

  let concrete_api =
    let open External.Impl in
    let int_to_four_hex v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (int_to_four_hex v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let octal_to_decimal v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (octal_to_decimal v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let to_precision v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (to_precision (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let to_exponential v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (to_exponential (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let to_fixed v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (to_fixed (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let from_char_code v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (from_char_code v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let to_char_code v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (to_char_code v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let s_len v = ok @@ Smtml.Expr.unop Ty_str Length v in
    let s_concat v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (s_concat v)
      | List l -> ok @@ Smtml.Expr.naryop Ty_str Concat l
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let s_nth v1 v2 = ok @@ Smtml.Expr.binop Ty_str At v1 v2 in
    let s_substr v1 v2 v3 =
      ok @@ Smtml.Expr.triop Ty_str String_extract v1 v2 v3
    in
    let from_char_code_u v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (from_char_code_u v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let to_char_code_u v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (to_char_code_u v)
      | _ -> ok (Smtml.Expr.cvtop Ty_str String_to_code v)
      (* failure "%s: invalid argument" __FUNCTION__ *)
    in
    let to_lower_case v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (to_lower_case v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let to_upper_case v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (to_upper_case v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let trim v = ok @@ Smtml.Expr.unop Ty_str Trim v in
    let s_len_u v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (s_len_u v)
      | _ -> ok @@ Smtml.Expr.unop Ty_str Length v
    in
    let s_nth_u v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (s_nth_u (v1, v2))
      | _ -> ok @@ Smtml.Expr.binop Ty_str At v1 v2
    in
    let s_split v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> (
        match s_split (v1, v2) with
        | Smtml.Value.List lst ->
          let lst = List.map Smtml.Expr.value lst in
          ok @@ Smtml.Expr.list lst
        | _ -> assert false )
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let s_substr_u v1 v2 v3 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2, Smtml.Expr.view v3) with
      | (Val v1, Val v2, Val v3) -> ok_v (s_substr_u (v1, v2, v3))
      | _ -> ok @@ Smtml.Expr.triop Ty_str String_extract v1 v2 v3
    in
    let s_is_prefix prefix str =
      match (Smtml.Expr.view prefix, Smtml.Expr.view str) with
      | (Val prefix, Val str) -> ok_v (s_is_prefix prefix str)
      | _ -> ok @@ Smtml.Expr.binop Ty_str String_prefix prefix str
    in
    let s_is_suffix suffix str =
      match (Smtml.Expr.view suffix, Smtml.Expr.view str) with
      | (Val suffix, Val str) -> ok_v (s_is_suffix suffix str)
      | _ -> ok @@ Smtml.Expr.binop Ty_str String_suffix suffix str
    in
    let array_len v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (array_len v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let array_make v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (array_make (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let array_nth v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (array_nth (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let array_set v1 v2 v3 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2, Smtml.Expr.view v3) with
      | (Val v1, Val v2, Val v3) -> ok_v (array_set (v1, v2, v3))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let l_len v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (l_len v)
      | List _ -> ok @@ Smtml.Expr.unop Ty_list Length v
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let l_nth v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (l_nth (v1, v2))
      | (List _, _) -> ok @@ Smtml.Expr.binop Ty_list At v1 v2
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let l_add v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (l_add (v1, v2))
      | (List _, _) ->
        ok @@ Smtml.Expr.binop Ty_list List_append v1 @@ Smtml.Expr.list [ v2 ]
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let l_prepend v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (l_prepend (v1, v2))
      | (_, List _) -> ok @@ Smtml.Expr.binop Ty_list List_cons v1 v2
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let l_concat v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (l_concat (v1, v2))
      | _ -> ok @@ Smtml.Expr.binop Ty_list List_append v1 v2
    in
    let l_set v1 v2 v3 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2, Smtml.Expr.view v3) with
      | (Val v1, Val v2, Val v3) -> ok_v (l_set (v1, v2, v3))
      | _ -> ok @@ Smtml.Expr.triop Ty_list List_set v1 v2 v3
    in
    let list_to_array v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (list_to_array v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let list_sort v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (list_sort v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let list_mem v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (list_mem (v1, v2))
      | (_, List lst) -> ok_v (if List.mem v1 lst then True else False)
      | _ ->
        failure "%s: invalid arguments: %a %a" __FUNCTION__ Smtml.Expr.pp v1
          Smtml.Expr.pp v2
    in
    let list_remove_last v =
      let rec aux_remove_lst = function
        | [] | [ _ ] -> []
        | hd :: tl -> hd :: aux_remove_lst tl
      in
      match Smtml.Expr.view v with
      | Val v -> ok_v (list_remove_last v)
      | List lst ->
        let lst' = aux_remove_lst lst in
        ok (Smtml.Expr.list lst')
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let list_remove v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (list_remove (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let list_remove_nth v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (list_remove_nth (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float_to_byte v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float_to_byte v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float32_to_le_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float32_to_le_bytes v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float32_to_be_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float32_to_be_bytes v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float64_to_le_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float64_to_le_bytes v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float64_to_be_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float64_to_be_bytes v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float32_from_le_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float32_from_le_bytes v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float32_from_be_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float32_from_be_bytes v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float64_from_le_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float64_from_le_bytes v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let float64_from_be_bytes v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (float64_from_be_bytes v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let bytes_to_string v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (bytes_to_string v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let int_to_be_bytes v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (int_to_be_bytes (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let int_from_le_bytes v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (int_from_le_bytes (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let uint_from_le_bytes v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (uint_from_le_bytes (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let log_2 v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (log_2 v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let log_e v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (log_e v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let log_10 v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (log_10 v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let sin v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (sin v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let cos v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (cos v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let tan v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (tan v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let sinh v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (sinh v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let cosh v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (cosh v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let tanh v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (tanh v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let asin v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (asin v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let acos v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (acos v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let atan v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (atan v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let atan2 v1 v2 =
      match (Smtml.Expr.view v1, Smtml.Expr.view v2) with
      | (Val v1, Val v2) -> ok_v (atan2 (v1, v2))
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let exp v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (exp v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let random v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (random v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let abs v = ok @@ Smtml.Expr.unop Ty_real Abs v in
    let sqrt v = ok @@ Smtml.Expr.unop Ty_real Sqrt v in
    let ceil v = ok @@ Smtml.Expr.unop Ty_real Ceil v in
    let floor v = ok @@ Smtml.Expr.unop Ty_real Floor v in
    let trunc v = ok @@ Smtml.Expr.unop Ty_real Trunc v in
    let utf8_decode v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (utf8_decode v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let hex_decode v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (hex_decode v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let parse_number v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (parse_number v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let parse_string v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (parse_string v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let parse_date v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (parse_date v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let open_in v =
      match Smtml.Expr.view v with
      | Val (Str fpath) -> ok @@ Channel_utils.open_in channel_table fpath
      | _ -> failure "invalid file path: %a" Smtml.Expr.pp v
    in
    let open_out v =
      match Smtml.Expr.view v with
      | Val (Str fpath) -> ok @@ Channel_utils.open_out channel_table fpath
      | _ -> failure "invalid file path: %a" Smtml.Expr.pp v
    in
    let input_line v =
      let open Result in
      Choice.return
      @@
      let* ty = Channel_utils.find channel_table v in
      let* ic = Channel_utils.Type.get_in ty in
      match In_channel.input_line ic with
      | None -> Ok (Symbolic_value.mk_symbol "undefined")
      | Some line -> Ok (Smtml.Expr.value (Str line))
    in
    let input_all v =
      let open Result in
      Choice.return
      @@
      let* ty = Channel_utils.find channel_table v in
      let* ic = Channel_utils.Type.get_in ty in
      Ok (Smtml.Expr.value (Str (In_channel.input_all ic)))
    in
    let output_string v str =
      let open Result in
      Choice.return
      @@
      let* ty = Channel_utils.find channel_table v in
      let* oc = Channel_utils.Type.get_out ty in
      let* () =
        match Smtml.Expr.view str with
        | Val (Str str) -> Ok (Out_channel.output_string oc str)
        | _ -> Error (`Failure "cannot write non-string object")
      in
      Ok (Symbolic_value.mk_symbol "undefined")
    in
    let close v =
      let open Result in
      Choice.return
      @@
      let* () = Channel_utils.close channel_table v in
      Ok (Symbolic_value.mk_symbol "undefined")
    in
    let file_exists v =
      match Smtml.Expr.view v with
      | Val v -> ok_v (file_exists v)
      | _ -> failure "%s: invalid argument" __FUNCTION__
    in
    let time () = ok_v (Real (Unix.gettimeofday ())) in
    of_array
      [| (* int *)
         ( "int_to_four_hex_external"
         , Extern_func (Func (Arg Res), int_to_four_hex) )
       ; ( "octal_to_decimal_external"
         , Extern_func (Func (Arg Res), octal_to_decimal) )
         (* float *)
       ; ( "to_precision_external"
         , Extern_func (Func (Arg (Arg Res)), to_precision) )
       ; ( "to_exponential_external"
         , Extern_func (Func (Arg (Arg Res)), to_exponential) )
       ; ("to_fixed_external", Extern_func (Func (Arg (Arg Res)), to_fixed))
         (* string *)
       ; ( "from_char_code_external"
         , Extern_func (Func (Arg Res), from_char_code) )
       ; ("to_char_code_external", Extern_func (Func (Arg Res), to_char_code))
       ; ("s_len_external", Extern_func (Func (Arg Res), s_len))
       ; ("s_concat_external", Extern_func (Func (Arg Res), s_concat))
       ; ("s_nth_external", Extern_func (Func (Arg (Arg Res)), s_nth))
       ; ( "s_substr_external"
         , Extern_func (Func (Arg (Arg (Arg Res))), s_substr) )
       ; ( "from_char_code_u_external"
         , Extern_func (Func (Arg Res), from_char_code_u) )
       ; ( "to_char_code_u_external"
         , Extern_func (Func (Arg Res), to_char_code_u) )
       ; ("to_lower_case_external", Extern_func (Func (Arg Res), to_lower_case))
       ; ("to_upper_case_external", Extern_func (Func (Arg Res), to_upper_case))
       ; ("trim_external", Extern_func (Func (Arg Res), trim))
       ; ("s_len_u_external", Extern_func (Func (Arg Res), s_len_u))
       ; ("s_nth_u_external", Extern_func (Func (Arg (Arg Res)), s_nth_u))
       ; ("s_split_external", Extern_func (Func (Arg (Arg Res)), s_split))
       ; ( "s_substr_u_external"
         , Extern_func (Func (Arg (Arg (Arg Res))), s_substr_u) )
       ; ( "s_is_prefix_external"
         , Extern_func (Func (Arg (Arg Res)), s_is_prefix) )
       ; ( "s_is_suffix_external"
         , Extern_func (Func (Arg (Arg Res)), s_is_suffix) )
         (* array *)
       ; ("a_len_external", Extern_func (Func (Arg Res), array_len))
       ; ("array_make_external", Extern_func (Func (Arg (Arg Res)), array_make))
       ; ("a_nth_external", Extern_func (Func (Arg (Arg Res)), array_nth))
       ; ("a_set_external", Extern_func (Func (Arg (Arg (Arg Res))), array_set))
         (* list *)
       ; ("l_len_external", Extern_func (Func (Arg Res), l_len))
       ; ("l_nth_external", Extern_func (Func (Arg (Arg Res)), l_nth))
       ; ("l_add_external", Extern_func (Func (Arg (Arg Res)), l_add))
       ; ("l_prepend_external", Extern_func (Func (Arg (Arg Res)), l_prepend))
       ; ("l_concat_external", Extern_func (Func (Arg (Arg Res)), l_concat))
       ; ("l_set_external", Extern_func (Func (Arg (Arg (Arg Res))), l_set))
       ; ("list_to_array_external", Extern_func (Func (Arg Res), list_to_array))
       ; ("l_sort_external", Extern_func (Func (Arg Res), list_sort))
       ; ("in_list_external", Extern_func (Func (Arg (Arg Res)), list_mem))
       ; ( "l_remove_last_external"
         , Extern_func (Func (Arg Res), list_remove_last) )
       ; ("l_remove_external", Extern_func (Func (Arg (Arg Res)), list_remove))
       ; ( "l_remove_nth_external"
         , Extern_func (Func (Arg (Arg Res)), list_remove_nth) )
         (* byte *)
       ; ("float_to_byte_external", Extern_func (Func (Arg Res), float_to_byte))
       ; ( "float32_to_le_bytes_external"
         , Extern_func (Func (Arg Res), float32_to_le_bytes) )
       ; ( "float32_to_be_bytes_external"
         , Extern_func (Func (Arg Res), float32_to_be_bytes) )
       ; ( "float64_to_le_bytes_external"
         , Extern_func (Func (Arg Res), float64_to_le_bytes) )
       ; ( "float64_to_be_bytes_external"
         , Extern_func (Func (Arg Res), float64_to_be_bytes) )
       ; ( "float32_from_le_bytes_external"
         , Extern_func (Func (Arg Res), float32_from_le_bytes) )
       ; ( "float32_from_be_bytes_external"
         , Extern_func (Func (Arg Res), float32_from_be_bytes) )
       ; ( "float64_from_le_bytes_external"
         , Extern_func (Func (Arg Res), float64_from_le_bytes) )
       ; ( "float64_from_be_bytes_external"
         , Extern_func (Func (Arg Res), float64_from_be_bytes) )
       ; ( "bytes_to_string_external"
         , Extern_func (Func (Arg Res), bytes_to_string) )
       ; ( "int_to_be_bytes_external"
         , Extern_func (Func (Arg (Arg Res)), int_to_be_bytes) )
       ; ( "int_from_le_bytes_external"
         , Extern_func (Func (Arg (Arg Res)), int_from_le_bytes) )
       ; ( "uint_from_le_bytes_external"
         , Extern_func (Func (Arg (Arg Res)), uint_from_le_bytes) )
         (* math *)
       ; ("log_2_external", Extern_func (Func (Arg Res), log_2))
       ; ("log_e_external", Extern_func (Func (Arg Res), log_e))
       ; ("log_10_external", Extern_func (Func (Arg Res), log_10))
       ; ("sin_external", Extern_func (Func (Arg Res), sin))
       ; ("cos_external", Extern_func (Func (Arg Res), cos))
       ; ("tan_external", Extern_func (Func (Arg Res), tan))
       ; ("sinh_external", Extern_func (Func (Arg Res), sinh))
       ; ("cosh_external", Extern_func (Func (Arg Res), cosh))
       ; ("tanh_external", Extern_func (Func (Arg Res), tanh))
       ; ("asin_external", Extern_func (Func (Arg Res), asin))
       ; ("acos_external", Extern_func (Func (Arg Res), acos))
       ; ("atan_external", Extern_func (Func (Arg Res), atan))
       ; ("atan2_external", Extern_func (Func (Arg (Arg Res)), atan2))
       ; ("exp_external", Extern_func (Func (Arg Res), exp)) (* parse *)
       ; ("random_external", Extern_func (Func (Arg Res), random)) (* parse *)
       ; ("abs_external", Extern_func (Func (Arg Res), abs)) (* parse *)
       ; ("sqrt_external", Extern_func (Func (Arg Res), sqrt)) (* parse *)
       ; ("ceil_external", Extern_func (Func (Arg Res), ceil)) (* parse *)
       ; ("floor_external", Extern_func (Func (Arg Res), floor)) (* parse *)
       ; ("trunc_external", Extern_func (Func (Arg Res), trunc)) (* parse *)
       ; ("utf8_decode_external", Extern_func (Func (Arg Res), utf8_decode))
       ; ("hex_decode_external", Extern_func (Func (Arg Res), hex_decode))
       ; ("parse_number_external", Extern_func (Func (Arg Res), parse_number))
       ; ("parse_string_external", Extern_func (Func (Arg Res), parse_string))
       ; ("parse_date_external", Extern_func (Func (Arg Res), parse_date))
       ; ("open_in_external", Extern_func (Func (Arg Res), open_in))
       ; ("open_out_external", Extern_func (Func (Arg Res), open_out))
       ; ("input_line_external", Extern_func (Func (Arg Res), input_line))
       ; ("input_all_external", Extern_func (Func (Arg Res), input_all))
       ; ( "output_string_external"
         , Extern_func (Func (Arg (Arg Res)), output_string) )
       ; ("close_external", Extern_func (Func (Arg Res), close))
       ; ("file_exists_external", Extern_func (Func (Arg Res), file_exists))
       ; ("time_external", Extern_func (Func (UArg Res), time))
      |]

  let symbolic_api filename =
    let open Extern_func in
    let non_empty v =
      match Smtml.Expr.view v with
      | Val (Str "") -> Ok (fresh_x ())
      | Val (Str s) -> Ok s
      | _ ->
        Error
          (`Failure
             (Fmt.str "'%a' is not a valid string symbol" Symbolic_value.pp v)
          )
    in
    let str_symbol (x : value) =
      let open Result in
      Choice.return
      @@
      let* x = non_empty x in
      Ok (Smtml.Expr.symbol (Smtml.Symbol.make Ty_str x))
    in
    let int_symbol (x : value) =
      let open Result in
      Choice.return
      @@
      let* x = non_empty x in
      Ok (Smtml.Expr.symbol (Smtml.Symbol.make Ty_int x))
    in
    let flt_symbol (x : value) =
      let open Result in
      Choice.return
      @@
      let* x = non_empty x in
      Ok (Smtml.Expr.symbol (Smtml.Symbol.make Ty_real x))
    in
    let bool_symbol (x : value) =
      let open Result in
      Choice.return
      @@
      let* x = non_empty x in
      Ok (Smtml.Expr.symbol (Smtml.Symbol.make Ty_bool x))
    in
    let lift_symbols (x : value) =
      match Smtml.Expr.view x with
      | List symbols -> Choice.from_list @@ List.map Result.ok symbols
      | _ -> assert false
    in
    let is_symbolic (n : value) =
      ok_v (if Symbolic_value.is_symbolic n then True else False)
    in
    let is_number (n : value) =
      ok_v
        (* TODO:x check is this is right *)
        ( match Smtml.Expr.ty n with
        | Ty_int | Ty_real -> True
        | _ -> False )
    in
    let is_sat (e : value) =
      let open Choice in
      let* b = Choice.branch e in
      ok_v (if b then True else False)
    in
    (* TODO: Maybe we can refactor `exec`, `eval`, and `read_file` to use `abort` instead when reaching a sensitive sink with a symbolic expression *)
    let exec (e : value) =
      match Smtml.Expr.view e with
      | Val _ -> ok @@ Symbolic_value.mk_symbol "undefined"
      | _ -> error (`Exec_failure e)
    in
    let eval (e : value) =
      match Smtml.Expr.view e with
      | Val _ -> ok @@ Symbolic_value.mk_symbol "undefined"
      | _ -> error (`Eval_failure e)
    in
    let read_file (e : value) =
      match Smtml.Expr.view e with
      | Val _ -> ok @@ Symbolic_value.mk_symbol "undefined"
      | _ -> error (`ReadFile_failure e)
    in
    let abort (e : value) = error (`Abort (Symbolic_value.to_string e)) in
    let assume (e : value) =
      match Smtml.Expr.view e with
      | Val False -> Choice.stop
      | Val True -> ok @@ Symbolic_value.mk_symbol "undefined"
      | _ ->
        Choice.with_mutable_thread @@ fun thread ->
        (Ok (Symbolic_value.mk_symbol "undefined"), Thread.add_pc thread e)
    in
    let evaluate (e : value) =
      Choice.with_thread @@ fun thread ->
      let pc = Smtml.Expr.Set.add e @@ Thread.pc thread in
      let solver = Thread.solver thread in
      assert (Choice_monad.is_sat @@ Solver.check_set solver pc);
      Ok (Solver.get_value solver e)
    in
    let optimize target opt e pc =
      Optimizer.push opt;
      Optimizer.add opt pc;
      let v = target opt e in
      Optimizer.pop opt;
      v
    in
    let maximize (e : value) =
      Choice.with_thread @@ fun thread ->
      let pc = Smtml.Expr.Set.to_list @@ Thread.pc thread in
      let opt = Thread.optimizer thread in
      let v = optimize Optimizer.maximize opt e pc in
      match v with
      | Some v -> Ok (Smtml.Expr.value v)
      | None ->
        (* TODO: Error here *)
        assert false
    in
    let minimize (e : value) =
      Choice.with_thread @@ fun thread ->
      let pc = Smtml.Expr.Set.to_list @@ Thread.pc thread in
      let opt = Thread.optimizer thread in
      let v = optimize Optimizer.minimize opt e pc in
      match v with
      | Some v -> Ok (Smtml.Expr.value v)
      | None ->
        (* TODO: Error here *)
        assert false
    in
    let print (v : Symbolic_value.value) =
      Logs.app (fun k -> k "extern print: %a@." Symbolic_value.pp v);
      ok_v (App (`Op "symbol", [ Str "undefined" ]))
    in
    (* TODO: The following functions where optimizations merged from the
       `trunk` branch. Check if we can integrate this with the concrete API. *)
    let str_replace (s : Symbolic_value.value) (t : Symbolic_value.value)
      (t' : Symbolic_value.value) =
      let t =
        match Smtml.Expr.view t with
        | Val (Str input) ->
          if Re.string_match g_replace_re input 0 then
            Smtml.Expr.value (Str (Re.matched_group 1 input))
          else t
        | _ -> t
      in
      ok @@ Smtml.Expr.triop Ty_str String_replace s t t'
    in
    let str_indexof (s : Symbolic_value.value) (t : Symbolic_value.value)
      (i : Symbolic_value.value) =
      ok
      @@ Smtml.Expr.cvtop Ty_real Reinterpret_int
      @@ Smtml.Expr.triop Ty_str String_index s t
      @@ Smtml.Expr.cvtop Ty_int Reinterpret_float i
    in
    let str_lastIndexOf (s : Symbolic_value.value) (t : Symbolic_value.value) =
      ok
      @@ Smtml.Expr.cvtop Ty_real Reinterpret_int
      @@ Smtml.Expr.binop Ty_str String_last_index s t
    in
    let str_sub (s : Symbolic_value.value) (start : Symbolic_value.value)
      (len : Symbolic_value.value) =
      ok @@ Smtml.Expr.triop Ty_str String_extract s start len
    in
    let str_parse_int (str : Symbolic_value.value) =
      ok
      @@ Smtml.Expr.cvtop Ty_real Reinterpret_int
      @@ Smtml.Expr.cvtop Ty_str String_to_int str
    in
    let str_split (s : Symbolic_value.value) (r : Symbolic_value.value) =
      Choice.with_mutable_thread @@ fun thread ->
      let x1 = Smtml.Expr.symbol @@ Smtml.Symbol.make Ty_str @@ fresh_x () in
      let x2 = Smtml.Expr.symbol @@ Smtml.Symbol.make Ty_str @@ fresh_x () in
      let cond =
        Smtml.Expr.relop Ty_bool Eq s
        @@ Smtml.Expr.naryop Ty_str Concat [ x1; r; x2 ]
      in
      let result = Symbolic_value.(mk_list [ x1; x2 ]) in
      (Ok result, Thread.add_pc thread cond)
    in
    let str_match (s : Symbolic_value.value) =
      Choice.with_mutable_thread @@ fun thread ->
      let x = Smtml.Expr.symbol @@ Smtml.Symbol.make Ty_str @@ fresh_x () in
      let cond = Smtml.Expr.binop Ty_str String_contains s x in
      (Ok (Symbolic_value.mk_list [ x ]), Thread.add_pc thread cond)
    in
    let dirname () =
      let (dirname, _) = Fpath.split_base filename in
      ok_v @@ Str (Fpath.to_string dirname)
    in
    let filename () =
      let filename = Fpath.to_string filename in
      ok_v @@ Str filename
    in
    let process_cwd () =
      match Bos.OS.Dir.current () with
      | Ok fpath -> ok_v @@ Str (Fpath.to_string fpath)
      | Error (`Msg err) ->
        Logs.err (fun k -> k "process_cwd: %s" err);
        ok @@ Symbolic_value.mk_symbol "undefined"
    in
    let are_lzs_on () = if !allow_lazy_values then ok_v True else ok_v False in
    of_array
      [| ("str_symbol", Extern_func (Func (Arg Res), str_symbol))
       ; ("int_symbol", Extern_func (Func (Arg Res), int_symbol))
       ; ("flt_symbol", Extern_func (Func (Arg Res), flt_symbol))
       ; ("bool_symbol", Extern_func (Func (Arg Res), bool_symbol))
       ; ("lift_symbols", Extern_func (Func (Arg Res), lift_symbols))
       ; ("is_symbolic", Extern_func (Func (Arg Res), is_symbolic))
       ; ("is_number", Extern_func (Func (Arg Res), is_number))
       ; ("is_sat", Extern_func (Func (Arg Res), is_sat))
       ; ("exec", Extern_func (Func (Arg Res), exec))
       ; ("eval", Extern_func (Func (Arg Res), eval))
       ; ("readFile", Extern_func (Func (Arg Res), read_file))
       ; ("abort", Extern_func (Func (Arg Res), abort))
       ; ("assume", Extern_func (Func (Arg Res), assume))
       ; ("evaluate", Extern_func (Func (Arg Res), evaluate))
       ; ("maximize", Extern_func (Func (Arg Res), maximize))
       ; ("minimize", Extern_func (Func (Arg Res), minimize))
       ; ("value", Extern_func (Func (Arg Res), print))
       ; ("str_replace", Extern_func (Func (Arg (Arg (Arg Res))), str_replace))
       ; ("str_indexOf", Extern_func (Func (Arg (Arg (Arg Res))), str_indexof))
       ; ("str_lastIndexOf", Extern_func (Func (Arg (Arg Res)), str_lastIndexOf))
       ; ("str_sub", Extern_func (Func (Arg (Arg (Arg Res))), str_sub))
       ; ("str_parseInt", Extern_func (Func (Arg Res), str_parse_int))
       ; ("str_match", Extern_func (Func (Arg Res), str_match))
       ; ("__dirname", Extern_func (Func (UArg Res), dirname))
       ; ("__filename", Extern_func (Func (UArg Res), filename))
       ; ("process_cwd_external", Extern_func (Func (UArg Res), process_cwd))
       ; ("summ_string_split", Extern_func (Func (Arg (Arg Res)), str_split))
       ; ("are_lzs_on", Extern_func (Func (UArg Res), are_lzs_on))
      |]
end
