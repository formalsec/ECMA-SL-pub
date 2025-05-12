open Test

let test_congruency = Typing.test_congruency

(* ========== Any Type ========== *)

let%test "congruency_any_eq" = test_congruency (t_any, t_any) (Ok ())

let%test "congruency_any_ref" = test_congruency (t_any, t_int) (Ok ())

let%test "congruency_any_src" = test_congruency (t_int, t_any) (Ok ())

(* ========== Unknown Type ========== *)

let%test "congruency_unknown_eq" = test_congruency (t_unknown, t_unknown) (Ok ())

let%test "congruency_unknown_ref" =
  test_congruency (t_unknown, t_int) (Error [ BadCongruency (t_unknown, t_int) ])

let%test "congruency_unknown_src" =
  test_congruency (t_int, t_unknown) (Error [ BadCongruency (t_int, t_unknown) ])

(* ========== Never Type ========== *)

let%test "congruency_never_eq" = test_congruency (t_never, t_never) (Ok ())

let%test "congruency_never_ref" =
  test_congruency (t_never, t_int) (Error [ BadCongruency (t_never, t_int) ])

let%test "congruency_never_src" =
  test_congruency (t_int, t_never) (Error [ BadCongruency (t_int, t_never) ])

(* ========== Undefined Type ========== *)

let%test "congruency_undefined_eq" =
  test_congruency (t_undefined, t_undefined) (Ok ())

let%test "congruency_undefined_ref" =
  test_congruency (t_undefined, t_int)
    (Error [ BadCongruency (t_undefined, t_int) ])

let%test "congruency_undefined_src" =
  test_congruency (t_int, t_undefined)
    (Error [ BadCongruency (t_int, t_undefined) ])

(* ========== Null Type ========== *)

let%test "congruency_null_eq" = test_congruency (t_null, t_null) (Ok ())

let%test "congruency_null_ref" =
  test_congruency (t_null, t_int) (Error [ BadCongruency (t_null, t_int) ])

let%test "congruency_null_src" =
  test_congruency (t_int, t_null) (Error [ BadCongruency (t_int, t_null) ])

(* ========== Void Type ========== *)

let%test "congruency_void_eq" = test_congruency (t_void, t_void) (Ok ())

let%test "congruency_void_ref" =
  test_congruency (t_void, t_int) (Error [ BadCongruency (t_void, t_int) ])

let%test "congruency_void_src" =
  test_congruency (t_int, t_void) (Error [ BadCongruency (t_int, t_void) ])

(* ========== Int Type ========== *)

let%test "congruency_int_eq" = test_congruency (t_int, t_int) (Ok ())

let%test "congruency_int_ref" =
  test_congruency (t_int, t_null) (Error [ BadCongruency (t_int, t_null) ])

let%test "congruency_int_src" =
  test_congruency (t_null, t_int) (Error [ BadCongruency (t_null, t_int) ])

let%test "congruency_int_literal" =
  test_congruency
    (t_int, lt_integer 10)
    (Error [ BadCongruency (t_int, lt_integer 10) ])

(* ========== Float Type ========== *)

let%test "congruency_float_eq" = test_congruency (t_float, t_float) (Ok ())

let%test "congruency_float_ref" =
  test_congruency (t_float, t_null) (Error [ BadCongruency (t_float, t_null) ])

let%test "congruency_float_src" =
  test_congruency (t_null, t_float) (Error [ BadCongruency (t_null, t_float) ])

let%test "congruency_float_literal" =
  test_congruency
    (t_float, lt_float 10.1)
    (Error [ BadCongruency (t_float, lt_float 10.1) ])

(* ========== String Type ========== *)

let%test "congruency_string_eq" = test_congruency (t_string, t_string) (Ok ())

let%test "congruency_string_ref" =
  test_congruency (t_string, t_null) (Error [ BadCongruency (t_string, t_null) ])

let%test "congruency_string_src" =
  test_congruency (t_null, t_string) (Error [ BadCongruency (t_null, t_string) ])

let%test "congruency_string_literal" =
  test_congruency
    (t_string, lt_string "abc")
    (Error [ BadCongruency (t_string, lt_string "abc") ])

(* ========== Boolean Type ========== *)

let%test "congruency_boolean_eq" = test_congruency (t_boolean, t_boolean) (Ok ())

let%test "congruency_boolean_ref" =
  test_congruency (t_boolean, t_null)
    (Error [ BadCongruency (t_boolean, t_null) ])

let%test "congruency_boolean_src" =
  test_congruency (t_null, t_boolean)
    (Error [ BadCongruency (t_null, t_boolean) ])

let%test "congruency_boolean_literal" =
  test_congruency
    (t_boolean, lt_boolean true)
    (Error [ BadCongruency (t_boolean, lt_boolean true) ])

(* ========== Symbol Type ========== *)

let%test "congruency_symbol_eq" = test_congruency (t_symbol, t_symbol) (Ok ())

let%test "congruency_symbol_ref" =
  test_congruency (t_symbol, t_null) (Error [ BadCongruency (t_symbol, t_null) ])

let%test "congruency_symbol_src" =
  test_congruency (t_null, t_symbol) (Error [ BadCongruency (t_null, t_symbol) ])

let%test "congruency_symbol_literal" =
  test_congruency
    (t_symbol, lt_symbol "a")
    (Error [ BadCongruency (t_symbol, lt_symbol "a") ])

(* ========== Literal Type ========== *)

let%test "congruency_literal_eq" =
  test_congruency (lt_integer 10, lt_integer 10) (Ok ())

let%test "congruency_literal_badval" =
  test_congruency
    (lt_integer 10, lt_integer 20)
    (Error [ BadCongruency (lt_integer 10, lt_integer 20) ])

let%test "congruency_literal_badtype" =
  test_congruency
    (lt_integer 10, lt_string "abc")
    (Error [ BadCongruency (lt_integer 10, lt_string "abc") ])

(* ========== Object Types ========== *)

let%test "congruency_object_empty" =
  let otref = t_obj [] in
  let otsrc = t_obj [] in
  test_congruency (otref, otsrc) (Ok ())

let%test "congruency_object_one_field" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_obj [ t_fld "foo" t_int ] in
  test_congruency (otref, otsrc) (Ok ())

let%test "congruency_object_multiple_fields" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_congruency (otref, otsrc) (Ok ())

let%test "congruency_object_innerobj_field" =
  let otref_inner = t_obj [ t_fld "baz" t_string ] in
  let otsrc_inner = t_obj [ t_fld "baz" t_string ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" otref_inner ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" otsrc_inner ] in
  test_congruency (otref, otsrc) (Ok ())

let%test "congruency_object_missing_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); MissingField ~@"bar" ])

let%test "congruency_object_extra_field" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); ExtraField ~@"bar" ])

let%test "congruency_object_incompatible_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" t_boolean ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "congruency_object_incompatible_innerobj_field" =
  let otref_inner = t_obj [ t_fld "baz" t_string ] in
  let otsrc_inner = t_obj [ t_fld "baz" t_boolean ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" otref_inner ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" otsrc_inner ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (otref_inner, otsrc_inner)
       ; IncompatibleField ~@"baz"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "congruency_object_covariant_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_unknown ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_unknown, t_string)
       ] )

let%test "congruency_object_optional_field_eq" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  test_congruency (otref, otsrc) (Ok ())

let%test "congruency_object_optional_field_ref" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_string)
       ] )

let%test "congruency_object_optional_field_src" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadCongruency (t_string, t_union [ t_string; t_undefined ])
       ] )

let%test "congruency_object_optional_field_covariant" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_unknown ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_boolean ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_unknown, t_boolean)
       ] )

let%test "congruency_object_optional_field_different" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_boolean ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "congruency_object_optional_field_undefined" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" t_undefined ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_undefined)
       ] )

let%test "congruency_object_optional_field_missing" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); MissingField ~@"bar" ])

let%test "congruency_object_optional_field_incompatible" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" t_boolean ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_boolean)
       ] )

let%test "congruency_object_optional_field_union" =
  let union = t_union [ t_string; t_undefined ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" union ] in
  test_congruency (otref, otsrc) (Ok ())

let%test "congruency_object_summary_field_eq" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  test_congruency (otref, otsrc) (Ok ())

let%test "congruency_object_summary_field_ref" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); MissingSummaryField t_string ])

let%test "congruency_object_summary_field_src" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); ExtraSummaryField ])

let%test "congruency_object_summary_field_covariant" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_unknown ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_boolean ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleField ~@"*"
       ; BadCongruency (t_unknown, t_boolean)
       ] )

let%test "congruency_object_summary_field_different" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_boolean ] in
  test_congruency (otref, otsrc)
    (Error
       [ BadCongruency (otref, otsrc)
       ; IncompatibleField ~@"*"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "congruency_object_summary_field_compatible" =
  let extra = t_fld "bar" t_string in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); ExtraField ~@"bar" ])

let%test "congruency_object_summary_field_undefined" =
  let extra = t_fld "bar" t_undefined in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); ExtraField ~@"bar" ])

let%test "congruency_object_summary_field_incompatible" =
  let extra = t_fld "bar" t_boolean in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); ExtraField ~@"bar" ])

let%test "congruency_object_summary_field_union" =
  let extra = t_fld "bar" (t_union [ t_string; t_undefined ]) in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); ExtraField ~@"bar" ])

let%test "congruency_object_summary_field_option" =
  let extra = t_fld "bar" ~opt:true t_string in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_congruency (otref, otsrc)
    (Error [ BadCongruency (otref, otsrc); ExtraField ~@"bar" ])

(* ========== List Type ========== *)

let%test "congruency_list_eq" =
  let tref = t_list t_int in
  let tsrc = t_list t_int in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_list_incompatible" =
  let tref = t_list t_int in
  let tsrc = t_list t_null in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); BadCongruency (t_int, t_null) ])

let%test "congruency_list_congruency" =
  let tref = t_list t_unknown in
  let tsrc = t_list t_int in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); BadCongruency (t_unknown, t_int) ])

(* ========== Tuple Type ========== *)

let%test "congruency_tuple_eq" =
  let tref = t_tuple [ t_int; t_string ] in
  let tsrc = t_tuple [ t_int; t_string ] in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_tuple_incompatible" =
  let tref = t_tuple [ t_int; t_string ] in
  let tsrc = t_tuple [ t_int; t_boolean ] in
  test_congruency (tref, tsrc)
    (Error
       [ BadCongruency (tref, tsrc)
       ; IncompatibleElement 2
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "congruency_tuple_congruency" =
  let tref = t_tuple [ t_int; t_unknown ] in
  let tsrc = t_tuple [ t_int; t_string ] in
  test_congruency (tref, tsrc)
    (Error
       [ BadCongruency (tref, tsrc)
       ; IncompatibleElement 2
       ; BadCongruency (t_unknown, t_string)
       ] )

let%test "congruency_tuple_missing" =
  let tref = t_tuple [ t_int; t_string; t_boolean ] in
  let tsrc = t_tuple [ t_int; t_string ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); NExpectedElements (3, 2) ])

let%test "congruency_tuple_extra" =
  let tref = t_tuple [ t_int; t_string ] in
  let tsrc = t_tuple [ t_int; t_string; t_boolean ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); NExpectedElements (2, 3) ])

(* ========== Union Type ========== *)

let%test "congruency_union_eq" =
  let tref = t_union [ t_int; t_string ] in
  let tsrc = t_union [ t_int; t_string ] in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_union_order" =
  let tref = t_union [ t_int; t_string ] in
  let tsrc = t_union [ t_string; t_int ] in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_union_extra_ref" =
  let tref = t_union [ t_int; t_string; t_boolean ] in
  let tsrc = t_union [ t_int; t_string ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); BadCongruency (tsrc, t_boolean) ])

let%test "congruency_union_extra_src" =
  let tref = t_union [ t_int; t_string ] in
  let tsrc = t_union [ t_int; t_string; t_boolean ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); BadCongruency (tref, t_boolean) ])

let%test "congruency_union_incompatible" =
  let tref = t_union [ t_int; t_string ] in
  let tsrc = t_union [ t_int; t_boolean ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); BadCongruency (tref, t_boolean) ])

let%test "congruency_union_congruency" =
  let tref = t_union [ t_int; t_unknown ] in
  let tsrc = t_union [ t_int; t_string; t_boolean ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); BadCongruency (tref, t_string) ])

let%test "congruency_union_any_ref" =
  let tref = t_union [ t_int; t_any; t_null ] in
  let tsrc = t_union [ t_int; t_string; t_boolean ] in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_union_any_src" =
  let tref = t_union [ t_int; t_string; t_boolean ] in
  let tsrc = t_union [ t_int; t_any; t_null ] in
  test_congruency (tref, tsrc) (Ok ())

(* ========== Sigma Types ========== *)

let%test "congruency_sigma_one_case" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo ] in
  let otsrc = t_obj [ t_fld "type" foo ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_sigma "type" [ otsrc ] in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_sigma_two_cases" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otsrc_foo = t_obj [ t_fld "type" foo ] in
  let otref_bar = t_obj [ t_fld "type" bar ] in
  let otsrc_bar = t_obj [ t_fld "type" bar ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_sigma_two_cases_order" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otref_bar = t_obj [ t_fld "type" bar ] in
  let otsrc_foo = t_obj [ t_fld "type" foo ] in
  let otsrc_bar = t_obj [ t_fld "type" bar ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_bar; otsrc_foo ] in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_sigma_complex" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_string ] in
  let otsrc_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otsrc_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_string ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_congruency (tref, tsrc) (Ok ())

let%test "congruency_sigma_incompatible_discriminants" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo ] in
  let otsrc = t_obj [ t_fld "disc" foo ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_sigma "disc" [ otsrc ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); IncompatibleSigmaDiscriminant ])

let%test "congruency_sigma_missing_case" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otref_bar = t_obj [ t_fld "type" bar ] in
  let otsrc_foo = t_obj [ t_fld "type" foo ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); MissingSigmaCase bar ])

let%test "congruency_sigma_extra_case" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otsrc_foo = t_obj [ t_fld "type" foo ] in
  let otsrc_bar = t_obj [ t_fld "type" bar ] in
  let tref = t_sigma "type" [ otref_foo ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_congruency (tref, tsrc)
    (Error [ BadCongruency (tref, tsrc); ExtraSigmaCase bar ])

let%test "congruency_sigma_incompatible_case" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_string ] in
  let otsrc_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otsrc_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_boolean ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_congruency (tref, tsrc)
    (Error
       [ BadCongruency (tref, tsrc)
       ; IncompatibleSigmaCase bar
       ; BadCongruency (otref_bar, otsrc_bar)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "congruency_sigma_covariant_case_literal" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_objlit [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_objlit [ t_fld "type" bar; t_fld "bar" t_unknown ] in
  let otsrc_foo = t_objlit [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otsrc_bar = t_objlit [ t_fld "type" bar; t_fld "bar" t_string ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_congruency (tref, tsrc)
    (Error
       [ BadCongruency (tref, tsrc)
       ; IncompatibleSigmaCase bar
       ; BadCongruency (otref_bar, otsrc_bar)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_unknown, t_string)
       ] )

let%test "congruency_sigma_covariant_case_stored" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_objsto [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_objsto [ t_fld "type" bar; t_fld "bar" t_unknown ] in
  let otsrc_foo = t_objsto [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otsrc_bar = t_objsto [ t_fld "type" bar; t_fld "bar" t_string ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_congruency (tref, tsrc)
    (Error
       [ BadCongruency (tref, tsrc)
       ; IncompatibleSigmaCase bar
       ; BadCongruency (otref_bar, otsrc_bar)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_unknown, t_string)
       ] )
