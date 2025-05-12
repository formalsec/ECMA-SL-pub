open Test

(* Simple types *)

let%test "syntax_any_test" = Syntax.test "any" (Ok t_any)

let%test "syntax_unknown_test" = Syntax.test "unknown" (Ok t_unknown)

let%test "syntax_never_test" = Syntax.test "never" (Ok t_never)

let%test "syntax_undefined_test" = Syntax.test "undefined" (Ok t_undefined)

let%test "syntax_null_test" = Syntax.test "null" (Ok t_null)

let%test "syntax_void_test" = Syntax.test "void" (Ok t_void)

(* Primitive types *)

let%test "syntax_int_test" = Syntax.test "int" (Ok t_int)

let%test "syntax_float_test" = Syntax.test "float  " (Ok t_float)

let%test "syntax_string_test" = Syntax.test "string" (Ok t_string)

let%test "syntax_boolean_test" = Syntax.test "boolean" (Ok t_boolean)

let%test "syntax_symbol_test" = Syntax.test "symbol" (Ok t_symbol)

(* Literal types *)

let%test "syntax_integer_literal_type_test" =
  Syntax.test "10" (Ok (lt_integer 10))

let%test "syntax_float_literal_type_test" =
  Syntax.test "10.1" (Ok (lt_float 10.1))

let%test "syntax_string_literal_type_test" =
  Syntax.test "\"abc\"" (Ok (lt_string "abc"))

let%test "syntax_boolean_literal_type_test" =
  Syntax.test "true" (Ok (lt_boolean true))

let%test "syntax_symbol_literal_type_test" =
  Syntax.test "'a" (Ok (lt_symbol "a"))

(* Simple object types *)

let%test "syntax_empty_object_type_test" = Syntax.test "{}" (Ok (t_objlit []))

let%test "syntax_one_field_object_type_test" =
  Syntax.test "{ foo: int }" (Ok (t_objlit [ t_fld "foo" t_int ]))

let%test "syntax_two_field_object_type_test" =
  Syntax.test "{ foo: int, bar: string }"
    (Ok (t_objlit [ t_fld "foo" t_int; t_fld "bar" t_string ]))

let%test "syntax_inner_object_type_test" =
  Syntax.test "{ foo: { bar: string } }"
    (Ok (t_objlit [ t_fld "foo" (t_objlit [ t_fld "bar" t_string ]) ]))

(* Custom field object types *)

let%test "syntax_optional_field_object_type_test" =
  Syntax.test "{ foo?: int }" (Ok (t_objlit [ t_fld "foo" t_int ~opt:true ]))

let%test "syntax_summary_field_object_type_test" =
  Syntax.test "{ *: string }" (Ok (t_objlit [ t_fld "*" t_string ]))

let%test "syntax_complete_object_type_test" =
  let foo = t_fld "foo" t_int in
  let bar = t_fld "bar" t_string ~opt:true in
  let baz = t_fld "*" t_boolean in
  Syntax.test "{ foo: int, bar?: string, *: boolean }"
    (Ok (t_objlit [ foo; bar; baz ]))

(* Bad object types *)

let%test "syntax_duplicate_diff_field_object_type_test" =
  Syntax.test "{ foo: int, foo: string }" (Error [ DuplicatedTField ~@"foo" ])

(* List types *)

let%test "syntax_integer_list_type_test" =
  Syntax.test "int[]" (Ok (t_list t_int))

let%test "syntax_float_list_type_test" =
  Syntax.test "float[]" (Ok (t_list t_float))

let%test "syntax_integer_double_list_type_test" =
  Syntax.test "int[][]" (Ok (t_list (t_list t_int)))

(* Tuple types *)

let%test "syntax_two_element_tuple_type_test" =
  Syntax.test "int * float" (Ok (t_tuple [ t_int; t_float ]))

let%test "syntax_three_element_tuple_type_test" =
  Syntax.test "int * float * string" (Ok (t_tuple [ t_int; t_float; t_string ]))

let%test "syntax_1st_inner_tuple_type_test" =
  let inner = t_tuple [ t_int; t_float ] in
  Syntax.test "(int * float) * string" (Ok (t_tuple [ inner; t_string ]))

let%test "syntax_2nd_inner_tuple_type_test" =
  let inner = t_tuple [ t_float; t_string ] in
  Syntax.test "int * (float * string)" (Ok (t_tuple [ t_int; inner ]))

let%test "syntax_two_inner_tuple_type_test" =
  let inner1 = t_tuple [ t_int; t_float ] in
  let inner2 = t_tuple [ t_string; t_boolean ] in
  Syntax.test "(int * float) * (string * boolean)"
    (Ok (t_tuple [ inner1; inner2 ]))

(* Union types *)

let%test "syntax_two_element_union_type_test" =
  Syntax.test "int | float" (Ok (t_union [ t_int; t_float ]))

let%test "syntax_three_element_union_type_test" =
  Syntax.test "int | float | string" (Ok (t_union [ t_int; t_float; t_string ]))

let%test "syntax_1st_inner_union_type_test" =
  let inner = t_union [ t_int; t_float ] in
  Syntax.test "(int | float) | string" (Ok (t_union [ inner; t_string ]))

let%test "syntax_2nd_inner_union_type_test" =
  let inner = t_union [ t_float; t_string ] in
  Syntax.test "int | (float | string)" (Ok (t_union [ t_int; inner ]))

let%test "syntax_two_inner_union_type_test" =
  let inner1 = t_union [ t_int; t_float ] in
  let inner2 = t_union [ t_string; t_boolean ] in
  Syntax.test "(int | float) | (string | boolean)"
    (Ok (t_union [ inner1; inner2 ]))

(* Combination of tuple, union, and list types *)

let%test "syntax_union_with_tuple_right_test" =
  Syntax.test "int | float * string"
    (Ok (t_union [ t_int; t_tuple [ t_float; t_string ] ]))

let%test "syntax_union_with_tuple_let_fld_test" =
  Syntax.test "int * float | string"
    (Ok (t_union [ t_tuple [ t_int; t_float ]; t_string ]))

let%test "syntax_tuple_with_list_test" =
  Syntax.test "int * float[]" (Ok (t_tuple [ t_int; t_list t_float ]))

let%test "syntax_union_with_list_test" =
  Syntax.test "int | float[]" (Ok (t_union [ t_int; t_list t_float ]))

let%test "syntax_list_of_tuple_test" =
  Syntax.test "(int * float)[]" (Ok (t_list (t_tuple [ t_int; t_float ])))

let%test "syntax_list_of_union_test" =
  Syntax.test "(int | float)[]" (Ok (t_list (t_union [ t_int; t_float ])))

(* Sigma types *)

let%test "syntax_no_bar_sigma_type_test" =
  Syntax.test "sigma[type] { type: \"foo\" }"
    (Ok (t_sigma "type" [ t_objlit [ t_fld "type" (lt_string "foo") ] ]))

let%test "syntax_one_case_sigma_type_test" =
  Syntax.test "sigma[type] | { type: \"foo\" }"
    (Ok (t_sigma "type" [ t_objlit [ t_fld "type" (lt_string "foo") ] ]))

let%test "syntax_one_case_two_fields_sigma_type_test" =
  let case_1 = t_objlit [ t_fld "type" (lt_string "foo"); t_fld "foo" t_int ] in
  Syntax.test "sigma[type] | { type: \"foo\", foo: int }"
    (Ok (t_sigma "type" [ case_1 ]))

let%test "syntax_two_case_sigma_type_test" =
  let case_1 = t_objlit [ t_fld "type" (lt_string "foo") ] in
  let case_2 = t_objlit [ t_fld "type" (lt_integer 10) ] in
  Syntax.test "sigma[type] { type: \"foo\" } | { type: 10 }"
    (Ok (t_sigma "type" [ case_1; case_2 ]))

(* Bad sigma types *)

let%test "syntax_unexpected_sigma_cases_test" =
  Syntax.test "sigma[type] int" (Error [ UnexpectedSigmaCase ])

let%test "syntax_unexpected_any_sigma_cases_test" =
  Syntax.test "sigma[type] any" (Error [ UnexpectedSigmaCase ])

let%test "syntax_unexpected_sigma_case_test" =
  Syntax.test "sigma[type] { type: \"foo\" } | int"
    (Error [ UnexpectedSigmaCase ])

let%test "syntax_missing_discriminant_sigma_type_test" =
  Syntax.test "sigma[type] { foo: \"foo\" }"
    (Error [ MissingSigmaDiscriminant ~@"type" ])

let%test "syntax_unexpected_discriminant_sigma_type_test" =
  Syntax.test "sigma[type] { type: int }" (Error [ UnexpectedSigmaDiscriminant ])

let%test "syntax_duplicated_discriminant_sigma_type_test" =
  Syntax.test "sigma[type] { type: \"foo\" } | { type: \"foo\" }"
    (Error [ DuplicatedSigmaDiscriminant (lt_string "foo") ])
