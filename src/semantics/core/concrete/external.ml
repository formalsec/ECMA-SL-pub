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

module Parsing' = Parsing
open Prelude
open EslBase
open EslSyntax

let ( let* ) = Result.bind

type store = Value.t Store.t

type heap = Value.t Heap.t

let op_err (_arg : int) (op_lbl : string) (rterr : Runtime_error.msg) : 'a =
  try Runtime_error.(throw ~src:Source.none rterr)
  with Runtime_error.Error err ->
    Runtime_error.(push (OpEvalExn op_lbl) err |> raise)

let unexpected_err (arg : int) (op_lbl : string) (msg : string) : 'a =
  op_err arg op_lbl (Unexpected msg)

let bad_arg_err (arg : int) (op_lbl : string) (types : string)
  (vals : Value.t list) : 'a =
  op_err arg op_lbl (BadOpArgs (types, vals))

let eval_build_ast_func = Base.make_name_generator "eval_func_"

let parseJS (prog : Prog.t) (code : string) : Value.t =
  let result =
    let* input = Bos.OS.File.tmp "ecmasl%seval_func.js" in
    let input = Fpath.to_string input in
    let* output = Bos.OS.File.tmp "ecmasl%seval_func.cesl" in
    let output = Fpath.to_string output in
    let eval_func_id = eval_build_ast_func () in
    Io.write_file input code;
    let js2ecmasl =
      EslJSParser.Api.cmd input (Some output) (Some eval_func_id)
    in
    let* () = Bos.OS.Cmd.run js2ecmasl in
    try
      let ast_func = Io.read_file output in
      let eval_func = Parsing'.parse_func ast_func in
      Hashtbl.replace (Prog.funcs prog) eval_func_id eval_func;
      Ok (Value.Str eval_func_id)
    with _ -> Log.fail "err in ParseJS"
  in
  match result with Ok v -> v | Error (`Msg err) -> Log.fail "%s" err

module Impl = struct
  (* FIXME: This leaks memory!! :( *)
  let arr_map = Hashtbl.create 128

  let arr_count = ref 0

  let int_to_four_hex (v : Value.t) : Value.t =
    let op_lbl = "int_to_four_hex_external" in
    match v with
    | Int i -> Str (Fmt.str "%04x" i)
    | _ -> bad_arg_err 1 op_lbl "integer" [ v ]

  let octal_to_decimal (v : Value.t) : Value.t =
    let op_lbl = "octal_to_decimal_external" in
    match v with
    | Int o ->
      let rec loop dec_value base temp =
        if temp = 0 then dec_value
        else
          let dec_value = dec_value + (temp mod 10 * base) in
          loop dec_value (base * 8) (temp / 10)
      in
      Int (loop 0 1 o)
    | _ -> bad_arg_err 1 op_lbl "integer" [ v ]

  let to_precision ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "to_precision_external" in
    match (v1, v2) with
    | (Real x, Int y) ->
      let z = Float.to_int (Float.log10 x) + 1 in
      if y < z then
        let exp = Float.log10 x in
        if Float.Infix.(exp >= 0.) then
          let num =
            Float.round
              (x /. (10. ** Float.trunc exp) *. (10. ** Float.of_int (y - 1)))
            /. (10. ** Float.of_int (y - 1))
          in
          if Float.is_integer num && y = 1 then
            Str (Fmt.str "%de+%d" (Float.to_int num) (Float.to_int exp))
          else Str (Fmt.str "%.12ge+%d" num (Float.to_int exp))
        else
          let num =
            Float.round
              (x /. (10. ** Float.floor exp) *. (10. ** Float.of_int (y - 1)))
            /. (10. ** Float.of_int (y - 1))
          in
          if Float.is_integer num && y = 1 then
            Str
              (Fmt.str "%de%d" (Float.to_int num)
                 (Float.to_int (Float.floor exp)) )
          else Str (Fmt.str "%.12ge%d" num (Float.to_int (Float.floor exp)))
      else
        let res =
          Float.round (x *. (10. ** float_of_int (y - 1)))
          /. (10. ** float_of_int (y - 1))
        in
        Str (Float.to_string res)
    | (Real _, _) -> bad_arg_err 2 op_lbl "(float, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(float, integer)" [ v1; v2 ]

  let to_exponential ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "to_exponential_external" in
    match (v1, v2) with
    | (Real x, Int y) ->
      let exp = Float.log10 x in
      if Float.Infix.(exp >= 0.) then
        let num =
          Float.round (x /. (10. ** Float.trunc exp) *. (10. ** Float.of_int y))
          /. (10. ** Float.of_int y)
        in
        if Float.is_integer num then
          Str (Fmt.str "%de+%d" (Float.to_int num) (Float.to_int exp))
        else Str (Fmt.str "%.12ge+%d" num (Float.to_int exp))
      else
        let num =
          Float.round (x /. (10. ** Float.floor exp) *. (10. ** Float.of_int y))
          /. (10. ** Float.of_int y)
        in
        if Float.is_integer num then
          Str
            (Fmt.str "%de%d" (Float.to_int num)
               (Float.to_int (Float.floor exp)) )
        else Str (Fmt.str "%.12ge%d" num (Float.to_int (Float.floor exp)))
    | (Real _, _) -> bad_arg_err 2 op_lbl "(float, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(float, integer)" [ v1; v2 ]

  let to_fixed ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "to_fixed_external" in
    match (v1, v2) with
    | (Real x, Int y) -> Str (Fmt.str "%0.*f" y x)
    | (Real _, _) -> bad_arg_err 2 op_lbl "(float, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(float, integer)" [ v1; v2 ]

  let from_char_code (v : Value.t) : Value.t =
    let op_lbl = "from_char_code_external" in
    try Smtml.Eval.cvtop Smtml.Ty.Ty_str String_from_code v
    with _ -> bad_arg_err 1 op_lbl "integer" [ v ]

  let to_char_code (v : Value.t) : Value.t =
    let op_lbl = "to_char_code_external" in
    try Smtml.Eval.cvtop Smtml.Ty.Ty_str String_to_code v
    with _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let s_len (v : Value.t) : Value.t =
    let op_lbl = "s_len_external" in
    try Smtml.Eval.unop Smtml.Ty.Ty_str Length v
    with _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let s_concat (v : Value.t) : Value.t =
    let op_lbl = "s_concat_external" in
    match v with
    | Value.List lst -> Smtml.Eval.naryop Smtml.Ty.Ty_str Concat lst
    | _ as v -> bad_arg_err 1 op_lbl "string list" [ v ]

  let s_nth (v1 : Value.t) (v2 : Value.t) : Value.t =
    let op_lbl = "s_nth_external" in
    match (v1, v2) with
    | (Value.Str _, Value.Int _) -> (
      try Smtml.Eval.binop Ty_str At v1 v2
      with _ -> unexpected_err 2 op_lbl "index out of bounds" )
    | (Str _, _) -> bad_arg_err 2 op_lbl "(string, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(string, integer)" [ v1; v2 ]

  let s_substr (v1 : Value.t) (v2 : Value.t) (v3 : Value.t) : Value.t =
    let op_lbl = "s_substr_external" in
    try Smtml.Eval.triop Ty_str String_extract v1 v2 v3
    with _ -> bad_arg_err 1 op_lbl "(string, integer, integer)" [ v1; v2; v3 ]

  let from_char_code_u (v : Value.t) : Value.t =
    let op_lbl = "from_char_code_u_external" in
    match v with
    | Int n -> Str (String_utils.from_char_code_u n)
    | _ -> bad_arg_err 1 op_lbl "integer" [ v ]

  let to_char_code_u (v : Value.t) : Value.t =
    let op_lbl = "to_char_code_u_external" in
    match v with
    | Str s -> Int (String_utils.to_char_code_u s)
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let to_lower_case (v : Value.t) : Value.t =
    let op_lbl = "to_lower_case_external" in
    match v with
    | Str s -> Str (String_utils.to_lower_case s)
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let to_upper_case (v : Value.t) : Value.t =
    let op_lbl = "to_upper_case_external" in
    match v with
    | Str s -> Str (String_utils.to_upper_case s)
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let trim (v : Value.t) : Value.t =
    let op_lbl = "trim_external" in
    match v with
    | Str s -> Str (String_utils.trim s)
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let s_len_u (v : Value.t) : Value.t =
    let op_lbl = "s_len_u_external" in
    match v with
    | Str s -> Int (String_utils.s_len_u s)
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let s_nth_u ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "s_nth_u_external" in
    match (v1, v2) with
    | (Str s, Int i) -> (
      try Str (String_utils.s_nth_u s i)
      with _ -> unexpected_err 2 op_lbl "index out of bounds" )
    | (Str _, _) -> bad_arg_err 2 op_lbl "(string, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(string, integer)" [ v1; v2 ]

  let s_split ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "s_split_external" in
    match (v1, v2) with
    | (_, Str "") -> unexpected_err 2 op_lbl "empty separator"
    | (Str str, Str sep) ->
      Value.List
        (List.map (fun s -> Value.Str s) (Re.split (Re.regexp sep) str))
    | (Str _, _) -> bad_arg_err 2 op_lbl "(string, string)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(string, string)" [ v1; v2 ]

  let s_substr_u ((v1, v2, v3) : Value.t * Value.t * Value.t) : Value.t =
    let op_lbl = "s_substr_u_external" in
    let err_msg = "(string, integer, integer)" in
    let arg_err i = bad_arg_err i op_lbl err_msg [ v1; v2; v3 ] in
    match (v1, v2, v3) with
    | (Str s, Int i, Int j) -> Str (String_utils.s_substr_u s i j)
    | (Str _, Int _, _) -> arg_err 3
    | (Str _, _, _) -> arg_err 2
    | _ -> arg_err 1

  let s_is_prefix (prefix : Value.t) (str : Value.t) =
    match Smtml.Eval.binop Ty_str String_prefix prefix str with
    | exception Smtml.Eval.TypeError { index; _ } ->
      bad_arg_err index "s_is_prefix_external" "(string, string)"
        [ prefix; str ]
    | v -> v

  let s_is_suffix (suffix : Value.t) (str : Value.t) =
    match Smtml.Eval.binop Ty_str String_suffix suffix str with
    | exception Smtml.Eval.TypeError { index; _ } ->
      bad_arg_err index "s_is_suffix_external" "(string, string)"
        [ suffix; str ]
    | v -> v

  let array_len (v : Value.t) : Value.t =
    let op_lbl = "a_len_external" in
    match v with
    | Int l -> (
      try
        let arr =
          match Hashtbl.find_opt arr_map l with
          | Some v -> v
          | None -> assert false
        in
        Value.Int (Array.length arr)
      with Not_found -> unexpected_err 1 op_lbl "array not found" )
    | _ -> bad_arg_err 1 op_lbl "array" [ v ]

  let array_make ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "array_make_external" in
    match (v1, v2) with
    | (Int n, v) ->
      if n > 0 then (
        let index = !arr_count in
        Hashtbl.add arr_map index (Array.make n v);
        arr_count := !arr_count + 1;
        Int index )
      else unexpected_err 1 op_lbl "non-positive array size"
    | _ -> bad_arg_err 1 op_lbl "(integer, any)" [ v1; v2 ]

  let array_nth ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "a_nth_external" in
    match (v1, v2) with
    | (Int l, Int i) -> (
      try
        let arr =
          match Hashtbl.find_opt arr_map l with
          | Some v -> v
          | None -> assert false
        in
        Array.get arr i
      with
      | Not_found -> unexpected_err 1 op_lbl "array not found"
      | _ -> unexpected_err 2 op_lbl "index out of bounds" )
    | (Int _, _) -> bad_arg_err 2 op_lbl "(integer, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(integer, integer)" [ v1; v2 ]

  let array_set ((v1, v2, v3) : Value.t * Value.t * Value.t) : Value.t =
    let op_lbl = "a_set_external" in
    match (v1, v2) with
    | (Int l, Int i) -> (
      try
        let arr =
          match Hashtbl.find_opt arr_map l with
          | Some v -> v
          | None -> assert false
        in
        Array.set arr i v3;
        Hashtbl.replace arr_map l arr;
        Value.Nothing
      with _ -> unexpected_err 2 op_lbl "index out of bounds" )
    | (Int _, _) ->
      bad_arg_err 2 op_lbl "(integer, integer, any)" [ v1; v2; v3 ]
    | _ -> bad_arg_err 1 op_lbl "(integer, integer, any)" [ v1; v2; v3 ]

  let l_len (v : Value.t) : Value.t =
    let op_lbl = "l_len_external" in
    try Smtml.Eval.unop Ty_list Length v
    with _ -> bad_arg_err 1 op_lbl "list" [ v ]

  let l_nth ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "l_nth_external" in
    match (v1, v2) with
    | (Value.List _, Value.Int _) -> (
      try Smtml.Eval.binop Ty_list At v1 v2
      with _ -> unexpected_err 2 op_lbl "index out of bounds" )
    | (Str _, _) -> bad_arg_err 2 op_lbl "(list, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(list, integer)" [ v1; v2 ]

  let l_add ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "l_add_external" in
    try Smtml.Eval.binop Ty_list List_append v1 (List [ v2 ])
    with _ -> bad_arg_err 2 op_lbl "(list, any)" [ v1; v2 ]

  let l_prepend ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "l_prepend_external" in
    match v2 with
    | Value.List _ -> Smtml.Eval.binop Ty_list List_cons v1 v2
    | _ -> bad_arg_err 1 op_lbl "(any, list)" [ v1; v2 ]

  let l_concat ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "l_concat_external" in
    match (v1, v2) with
    | (Value.List _, Value.List _) -> Smtml.Eval.binop Ty_list List_append v1 v2
    | (List _, _) -> bad_arg_err 2 op_lbl "(list, list)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(list, list)" [ v1; v2 ]

  let l_set ((v1, v2, v3) : Value.t * Value.t * Value.t) : Value.t =
    let op_lbl = "l_set_external" in
    try Smtml.Eval.triop Ty_list List_set v1 v2 v3
    with _ -> bad_arg_err 1 op_lbl "(list, integer, any)" [ v1; v2; v3 ]

  let list_to_array (v : Value.t) : Value.t =
    let op_lbl = "list_to_array_external" in
    match v with
    | List lst ->
      let arr = Array.of_list lst in
      let loc = !arr_count in
      Hashtbl.add arr_map loc arr;
      arr_count := !arr_count + 1;
      Int loc
    | _ -> bad_arg_err 1 op_lbl "list" [ v ]

  let string_concat_aux (lst : Value.t list) : string list option =
    let concat_f acc v =
      match (acc, v) with
      | (Some strs, Value.Str s) -> Some (strs @ [ s ])
      | _ -> None
    in
    List.fold_left concat_f (Some []) lst

  let list_sort (v : Value.t) : Value.t =
    let op_lbl = "l_sort_external" in
    let str_f s = Value.Str s in
    match v with
    | List lst -> (
      let strs = string_concat_aux lst in
      match strs with
      | Some strs -> List (List.map str_f (List.fast_sort String.compare strs))
      | None -> bad_arg_err 1 op_lbl "string list" [ v ] )
    | _ -> bad_arg_err 1 op_lbl "string list" [ v ]

  let list_mem ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "in_list_external" in
    match v2 with
    | List lst -> if List.mem v1 lst then Value.True else Value.False
    | _ -> bad_arg_err 2 op_lbl "(any, list)" [ v1; v2 ]

  let list_remove_last (v : Value.t) : Value.t =
    let op_lbl = "l_remove_last_external" in
    let rec aux_remove_last = function
      | [] | [ _ ] -> []
      | hd :: tl -> hd :: aux_remove_last tl
    in
    match v with
    | List lst -> List (aux_remove_last lst)
    | _ -> bad_arg_err 1 op_lbl "list" [ v ]

  let list_remove ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "l_remove_external" in
    match (v1, v2) with
    | (List lst, el) -> List (List.filter (fun v -> not (Value.equal el v)) lst)
    | _ -> bad_arg_err 1 op_lbl "(list, any)" [ v1; v2 ]

  let list_remove_nth ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "l_remove_nth_external" in
    let rec _remove_nth_aux lst i =
      match (lst, i) with
      | ([], _) -> unexpected_err 2 op_lbl "index out of bounds"
      | (_ :: tl, 0) -> tl
      | (hd :: tl, _) -> hd :: _remove_nth_aux tl (i - 1)
    in
    match (v1, v2) with
    | (List lst, Int i) -> List (_remove_nth_aux lst i)
    | (List _, _) -> bad_arg_err 2 op_lbl "(list, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(list, integer)" [ v1; v2 ]

  let float_to_byte (v : Value.t) : Value.t =
    let op_lbl = "float_to_byte_external" in
    match v with
    | Real x -> Int (Int64.to_int (Int64.bits_of_float x))
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let float32_to_le_bytes (v : Value.t) : Value.t =
    let op_lbl = "float32_to_le_bytes_external" in
    match v with
    | Real x ->
      let bytes = Byte_utils.float32_to_le_bytes x in
      let val_bytes = List.map (fun b -> Value.Int (Int32.to_int b)) bytes in
      List val_bytes
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let float32_to_be_bytes (v : Value.t) : Value.t =
    let op_lbl = "float32_to_be_bytes_external" in
    match v with
    | Real x ->
      let bytes = Byte_utils.float32_to_be_bytes x in
      let val_bytes = List.map (fun b -> Value.Int (Int32.to_int b)) bytes in
      List val_bytes
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let float64_to_le_bytes (v : Value.t) : Value.t =
    let op_lbl = "float64_to_le_bytes_external" in
    match v with
    | Real x ->
      let bytes = Byte_utils.float64_to_le_bytes x in
      let val_bytes = List.map (fun b -> Value.Int (Int64.to_int b)) bytes in
      List val_bytes
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let float64_to_be_bytes (v : Value.t) : Value.t =
    let op_lbl = "float64_to_be_bytes_external" in
    match v with
    | Real x ->
      let bytes = Byte_utils.float64_to_be_bytes x in
      let val_bytes = List.map (fun b -> Value.Int (Int64.to_int b)) bytes in
      List val_bytes
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let unpack_bytes_aux (op_lbl : string) (v : Value.t) : int array =
    let unpack_bt_f = function Value.Int i -> i | _ -> raise Exit in
    try
      match v with
      | Int l -> (
        try
          let bytes =
            match Hashtbl.find_opt arr_map l with
            | Some v -> v
            | None -> assert false
          in
          Array.map unpack_bt_f bytes
        with Not_found -> unexpected_err 1 op_lbl "array not found" )
      | _ -> bad_arg_err 1 op_lbl "byte array" [ v ]
    with _ -> bad_arg_err 1 op_lbl "byte array" [ v ]

  let float32_from_le_bytes (v : Value.t) : Value.t =
    let op_lbl = "float32_from_le_bytes_external" in
    let int_bytes = unpack_bytes_aux op_lbl v in
    let int32_bytes = Array.map Int32.of_int int_bytes in
    let f = Byte_utils.float32_from_le_bytes int32_bytes in
    Real f

  let float32_from_be_bytes (v : Value.t) : Value.t =
    let op_lbl = "float32_from_be_bytes_external" in
    let int_bytes = unpack_bytes_aux op_lbl v in
    let int32_bytes = Array.map Int32.of_int int_bytes in
    let f = Byte_utils.float32_from_be_bytes int32_bytes in
    Real f

  let float64_from_le_bytes (v : Value.t) : Value.t =
    let op_lbl = "float64_from_le_bytes_external" in
    let int_bytes = unpack_bytes_aux op_lbl v in
    let int64_bytes = Array.map Int64.of_int int_bytes in
    let f = Byte_utils.float64_from_le_bytes int64_bytes in
    Real f

  let float64_from_be_bytes (v : Value.t) : Value.t =
    let op_lbl = "float64_from_be_bytes_external" in
    let int_bytes = unpack_bytes_aux op_lbl v in
    let int64_bytes = Array.map Int64.of_int int_bytes in
    let f = Byte_utils.float64_from_be_bytes int64_bytes in
    Real f

  let bytes_to_string (v : Value.t) : Value.t =
    let op_lbl = "bytes_to_string_external" in
    let int_bytes = unpack_bytes_aux op_lbl v in
    let str_bytes = Array.map string_of_int int_bytes |> Array.to_list in
    let bytes_string = Fmt.str "[%s]" (String.concat "; " str_bytes) in
    Str bytes_string

  let int_to_be_bytes ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "int_to_be_bytes_external" in
    match (v1, v2) with
    | (Real x, Int n) ->
      let bytes = Byte_utils.int_to_be_bytes (x, n) in
      let val_bytes = List.map (fun b -> Value.Int b) bytes in
      List val_bytes
    | (Real _, _) -> bad_arg_err 2 op_lbl "(float, integer)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(float, integer)" [ v1; v2 ]

  let int_from_le_bytes ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "int_from_le_bytes_external" in
    let int_bytes =
      try unpack_bytes_aux op_lbl v1
      with _ -> bad_arg_err 1 op_lbl "(byte array, integer)" [ v1; v2 ]
    in
    match v2 with
    | Int n -> Real (Byte_utils.int_from_le_bytes (int_bytes, n))
    | _ -> bad_arg_err 2 op_lbl "(byte array, integer)" [ v1; v2 ]

  let uint_from_le_bytes ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "uint_from_le_bytes_external" in
    let int_bytes =
      try unpack_bytes_aux op_lbl v1
      with _ -> bad_arg_err 1 op_lbl "(byte array, integer)" [ v1; v2 ]
    in
    match v2 with
    | Int n -> Real (Byte_utils.uint_from_le_bytes (int_bytes, n))
    | _ -> bad_arg_err 2 op_lbl "(byte array, integer)" [ v1; v2 ]

  let random (v : Value.t) : Value.t =
    let op_lbl = "random_external" in
    match v with
    | Real f -> Real (Random.float f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let abs (v : Value.t) : Value.t =
    let op_lbl = "abs_external" in
    try Smtml.Eval.unop Smtml.Ty.Ty_real Abs v
    with _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let sqrt (v : Value.t) : Value.t =
    let op_lbl = "sqrt_external" in
    try Smtml.Eval.unop Smtml.Ty.Ty_real Sqrt v
    with _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let ceil (v : Value.t) : Value.t =
    let op_lbl = "ceil_external" in
    try Smtml.Eval.unop Smtml.Ty.Ty_real Ceil v
    with _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let floor (v : Value.t) : Value.t =
    let op_lbl = "floor_external" in
    try Smtml.Eval.unop Smtml.Ty.Ty_real Floor v
    with _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let trunc (v : Value.t) : Value.t =
    let op_lbl = "trunc_external" in
    try Smtml.Eval.unop Smtml.Ty.Ty_real Trunc v
    with _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let exp (v : Value.t) : Value.t =
    let op_lbl = "exp_external" in
    match v with
    | Real f -> Real (Float.exp f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let log_2 (v : Value.t) : Value.t =
    let op_lbl = "log_2_external" in
    match v with
    | Real f -> Real (Float.log2 f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let log_e (v : Value.t) : Value.t =
    let op_lbl = "log_e_external" in
    match v with
    | Real f -> Real (Float.log f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let log_10 (v : Value.t) : Value.t =
    let op_lbl = "log_10_external" in
    match v with
    | Real f -> Real (Float.log10 f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let sin (v : Value.t) : Value.t =
    let op_lbl = "sin_external" in
    match v with
    | Real f -> Real (Float.sin f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let cos (v : Value.t) : Value.t =
    let op_lbl = "cos_external" in
    match v with
    | Real f -> Real (Float.cos f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let tan (v : Value.t) : Value.t =
    let op_lbl = "tan_external" in
    match v with
    | Real f -> Real (Float.tan f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let sinh (v : Value.t) : Value.t =
    let op_lbl = "sinh_external" in
    match v with
    | Real f -> Real (Float.sinh f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let cosh (v : Value.t) : Value.t =
    let op_lbl = "cosh_external" in
    match v with
    | Real f -> Real (Float.cosh f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let tanh (v : Value.t) : Value.t =
    let op_lbl = "tanh_external" in
    match v with
    | Real f -> Real (Float.tanh f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let asin (v : Value.t) : Value.t =
    let op_lbl = "asin_external" in
    match v with
    | Real f -> Real (Float.asin f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let acos (v : Value.t) : Value.t =
    let op_lbl = "acos_external" in
    match v with
    | Real f -> Real (Float.acos f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let atan (v : Value.t) : Value.t =
    let op_lbl = "atan_external" in
    match v with
    | Real f -> Real (Float.atan f)
    | _ -> bad_arg_err 1 op_lbl "float" [ v ]

  let atan2 ((v1, v2) : Value.t * Value.t) : Value.t =
    let op_lbl = "atan2_external" in
    match (v1, v2) with
    | (Real f1, Real f2) -> Real (Float.atan2 f1 f2)
    | (Real _, _) -> bad_arg_err 2 op_lbl "(float, float)" [ v1; v2 ]
    | _ -> bad_arg_err 1 op_lbl "(float, float)" [ v1; v2 ]

  let utf8_decode (v : Value.t) : Value.t =
    let op_lbl = "utf8_decode_external" in
    match v with
    | Str s -> Str (String_utils.utf8decode s)
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let hex_decode (v : Value.t) : Value.t =
    let op_lbl = "hex_decode_external" in
    match v with
    | Str s -> Str (String_utils.hexdecode s)
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  (** * JSON number regex: https://stackoverflow.com/a/13340826/3049315 *
      Recognized Regexp constructs in OCaml Str: https://ocaml.org/api/Str.html
  *)
  let parse_number (v : Value.t) : Value.t =
    let op_lbl = "parse_number_external" in
    match v with
    | Str s ->
      let regex =
        Re.regexp
          "-?\\(0\\|[1-9][0-9]*\\)\\(\\.[0-9]+\\)?\\([eE][+-]?[0-9]+\\)?"
      in
      let matched = Re.string_match regex s 0 in
      if matched then Str (Re.matched_string s) else Str ""
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  (** * JSON string regex: https://stackoverflow.com/a/32155765/3049315 *)
  let parse_string (v : Value.t) : Value.t =
    let op_lbl = "parse_string_external" in
    match v with
    | Str s ->
      let regex =
        Re.regexp
          "\"\\(\\\\\\([\"\\\\\\/bfnrt]\\|u[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)\\|[^\"\\\\\000-\031\127]+\\)*\""
      in
      let matched = Re.string_match regex s 0 in
      if matched then Str (Re.matched_string s) else Str ""
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  let parse_date (v : Value.t) : Value.t =
    let op_lbl = "parse_date_external" in
    let remove_sign s = String.sub s 1 (String.length s - 1) in
    let signed_year year_neg year = if year_neg then -.year else year in
    let parse_date year_neg date =
      match date with
      | None -> Value.Real (-1.)
      | Some ([ year; month; day; hour; min; sec; msec ], tz) ->
        Value.List
          [ Value.Real (signed_year year_neg year)
          ; Value.Real month
          ; Value.Real day
          ; Value.Real hour
          ; Value.Real min
          ; Value.Real sec
          ; Value.Real msec
          ; Value.Str tz
          ]
      | _ -> unexpected_err 1 op_lbl "date format"
    in
    match v with
    | Str s ->
      let year_sign = s.[0] in
      if Char.equal year_sign '-' then
        remove_sign s |> Date_utils.parse_date |> parse_date true
      else if Char.equal year_sign '+' then
        remove_sign s |> Date_utils.parse_date |> parse_date false
      else Date_utils.parse_date s |> parse_date false
    | _ -> bad_arg_err 1 op_lbl "string" [ v ]

  (* TODO: Adapt `Channel_utils` to allow both the functorial and concrete interpreter to run these function *)
  let open_in _v = Fmt.failwith "TODO: open_in_external"

  let open_out _v = Fmt.failwith "TODO: open_out_external"

  let input_line _v = Fmt.failwith "TODO: input_line_external"

  let input_all _v = Fmt.failwith "TODO: input_all_external"

  let output_string _ptr _str = Fmt.failwith "TODO: output_string_external"

  let close _ptr = Fmt.failwith "TODO: close_external"

  let file_exists = function
    | Smtml.Value.Str path ->
      let exists =
        match Bos.OS.File.exists (Fpath.v path) with
        | Ok exists -> exists
        | Error (`Msg err) -> Fmt.failwith "%s" err
      in
      Smtml.Value.(if exists then True else False)
    | v -> bad_arg_err 1 "file_exists_external" "string" [ v ]

  let time () = Smtml.Value.(Real (Unix.gettimeofday ()))
end

include Impl

let execute (prog : Prog.t) (_store : 'a Store.t) (_heap : 'a Heap.t)
  (fn : Id.t') (vs : Value.t list) : Value.t =
  match (fn, vs) with
  | ("is_symbolic", _) -> Value.False
  | ("parseJS", [ Value.Str code ]) -> parseJS prog code
  (* int *)
  | ("int_to_four_hex_external", [ v ]) -> int_to_four_hex v
  | ("octal_to_decimal_external", [ v ]) -> octal_to_decimal v
  (* float *)
  | ("to_precision_external", [ v1; v2 ]) -> to_precision (v1, v2)
  | ("to_exponential_external", [ v1; v2 ]) -> to_exponential (v1, v2)
  | ("to_fixed_external", [ v1; v2 ]) -> to_fixed (v1, v2)
  (* string *)
  | ("from_char_code_external", [ v ]) -> from_char_code v
  | ("to_char_code_external", [ v ]) -> to_char_code v
  | ("s_len_external", [ v ]) -> s_len v
  | ("s_concat_external", [ v ]) -> s_concat v
  | ("s_nth_external", [ v1; v2 ]) -> s_nth v1 v2
  | ("s_substr_external", [ v1; v2; v3 ]) -> s_substr v1 v2 v3
  | ("from_char_code_u_external", [ v ]) -> from_char_code_u v
  | ("to_char_code_u_external", [ v ]) -> to_char_code_u v
  | ("to_lower_case_external", [ v ]) -> to_lower_case v
  | ("to_upper_case_external", [ v ]) -> to_upper_case v
  | ("trim_external", [ v ]) -> trim v
  | ("s_len_u_external", [ v ]) -> s_len_u v
  | ("s_nth_u_external", [ v1; v2 ]) -> s_nth_u (v1, v2)
  | ("s_split_external", [ v1; v2 ]) -> s_split (v1, v2)
  | ("s_substr_u_external", [ v1; v2; v3 ]) -> s_substr_u (v1, v2, v3)
  | ("s_is_prefix_external", [ prefix; str ]) -> s_is_prefix prefix str
  | ("s_is_suffix_external", [ suffix; str ]) -> s_is_suffix suffix str
  (* array *)
  | ("a_len_external", [ v ]) -> array_len v
  | ("array_make_external", [ v1; v2 ]) -> array_make (v1, v2)
  | ("a_nth_external", [ v1; v2 ]) -> array_nth (v1, v2)
  | ("a_set_external", [ v1; v2; v3 ]) -> array_set (v1, v2, v3)
  (* list *)
  | ("l_len_external", [ v ]) -> l_len v
  | ("l_nth_external", [ v1; v2 ]) -> l_nth (v1, v2)
  | ("l_add_external", [ v1; v2 ]) -> l_add (v1, v2)
  | ("l_prepend_external", [ v1; v2 ]) -> l_prepend (v1, v2)
  | ("l_concat_external", [ v1; v2 ]) -> l_concat (v1, v2)
  | ("l_set_external", [ v1; v2; v3 ]) -> l_set (v1, v2, v3)
  | ("list_to_array_external", [ v ]) -> list_to_array v
  | ("l_sort_external", [ v ]) -> list_sort v
  | ("in_list_external", [ v1; v2 ]) -> list_mem (v1, v2)
  | ("l_remove_last_external", [ v ]) -> list_remove_last v
  | ("l_remove_external", [ v1; v2 ]) -> list_remove (v1, v2)
  | ("l_remove_nth_external", [ v1; v2 ]) -> list_remove_nth (v1, v2)
  (* byte *)
  | ("float_to_byte_external", [ v ]) -> float_to_byte v
  | ("float32_to_le_bytes_external", [ v ]) -> float32_to_le_bytes v
  | ("float32_to_be_bytes_external", [ v ]) -> float32_to_be_bytes v
  | ("float64_to_le_bytes_external", [ v ]) -> float64_to_le_bytes v
  | ("float64_to_be_bytes_external", [ v ]) -> float64_to_be_bytes v
  | ("float32_from_le_bytes_external", [ v ]) -> float32_from_le_bytes v
  | ("float32_from_be_bytes_external", [ v ]) -> float32_from_be_bytes v
  | ("float64_from_le_bytes_external", [ v ]) -> float64_from_le_bytes v
  | ("float64_from_be_bytes_external", [ v ]) -> float64_from_be_bytes v
  | ("bytes_to_string_external", [ v ]) -> bytes_to_string v
  | ("int_to_be_bytes_external", [ v1; v2 ]) -> int_to_be_bytes (v1, v2)
  | ("int_from_le_bytes_external", [ v1; v2 ]) -> int_from_le_bytes (v1, v2)
  | ("uint_from_le_bytes_external", [ v1; v2 ]) -> uint_from_le_bytes (v1, v2)
  (* math *)
  | ("random_external", [ v ]) -> random v
  | ("abs_external", [ v ]) -> abs v
  | ("sqrt_external", [ v ]) -> sqrt v
  | ("ceil_external", [ v ]) -> ceil v
  | ("floor_external", [ v ]) -> floor v
  | ("trunc_external", [ v ]) -> trunc v
  | ("exp_external", [ v ]) -> exp v
  | ("log_2_external", [ v ]) -> log_2 v
  | ("log_e_external", [ v ]) -> log_e v
  | ("log_10_external", [ v ]) -> log_10 v
  | ("sin_external", [ v ]) -> sin v
  | ("cos_external", [ v ]) -> cos v
  | ("tan_external", [ v ]) -> tan v
  | ("sinh_external", [ v ]) -> sinh v
  | ("cosh_external", [ v ]) -> cosh v
  | ("tanh_external", [ v ]) -> tanh v
  | ("asin_external", [ v ]) -> asin v
  | ("acos_external", [ v ]) -> acos v
  | ("atan_external", [ v ]) -> atan v
  | ("atan2_external", [ v1; v2 ]) -> atan2 (v1, v2)
  (* parse *)
  | ("utf8_decode_external", [ v ]) -> utf8_decode v
  | ("hex_decode_external", [ v ]) -> hex_decode v
  | ("parse_number_external", [ v ]) -> parse_number v
  | ("parse_string_external", [ v ]) -> parse_string v
  | ("parse_date_external", [ v ]) -> parse_date v
  | ("open_in_external", [ v ]) -> open_in v
  | ("open_out_external", [ v ]) -> open_out v
  | ("input_line_external", [ v ]) -> input_line v
  | ("input_all_external", [ v ]) -> input_all v
  | ("output_string_external", [ v1; v2 ]) -> output_string v1 v2
  | ("close_external", [ v ]) -> close v
  | ("file_exists_external", [ v ]) -> file_exists v
  | ("time_external", _) -> time ()
  | _ ->
    Log.warn "UNKNOWN %s external function" fn;
    Value.App (`Op "symbol", [ Str "undefined" ])
