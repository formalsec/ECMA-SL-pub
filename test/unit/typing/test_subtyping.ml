open Test

let test_subtyping = Typing.test_subtyping

(* ========== Any Type ========== *)

let%test "subtyping_any_eq" = test_subtyping (t_any, t_any) (Ok ())

let%test "subtyping_any_ref" = test_subtyping (t_any, t_int) (Ok ())

let%test "subtyping_any_src" = test_subtyping (t_int, t_any) (Ok ())

(* ========== Unknown Type ========== *)

let%test "subtyping_unknown_eq" = test_subtyping (t_unknown, t_unknown) (Ok ())

let%test "subtyping_unknown_ref" = test_subtyping (t_unknown, t_int) (Ok ())

let%test "subtyping_unknown_src" =
  test_subtyping (t_int, t_unknown) (Error [ BadSubtyping (t_int, t_unknown) ])

(* ========== Never Type ========== *)

let%test "subtyping_never_eq" = test_subtyping (t_never, t_never) (Ok ())

let%test "subtyping_never_ref" =
  test_subtyping (t_never, t_int) (Error [ BadSubtyping (t_never, t_int) ])

let%test "subtyping_never_src" = test_subtyping (t_int, t_never) (Ok ())

(* ========== Undefined Type ========== *)

let%test "subtyping_undefined_eq" =
  test_subtyping (t_undefined, t_undefined) (Ok ())

let%test "subtyping_undefined_ref" =
  test_subtyping (t_undefined, t_int)
    (Error [ BadSubtyping (t_undefined, t_int) ])

let%test "subtyping_undefined_src" =
  test_subtyping (t_int, t_undefined)
    (Error [ BadSubtyping (t_int, t_undefined) ])

(* ========== Null Type ========== *)

let%test "subtyping_null_eq" = test_subtyping (t_null, t_null) (Ok ())

let%test "subtyping_null_ref" =
  test_subtyping (t_null, t_int) (Error [ BadSubtyping (t_null, t_int) ])

let%test "subtyping_null_src" =
  test_subtyping (t_int, t_null) (Error [ BadSubtyping (t_int, t_null) ])

(* ========== Void Type ========== *)

let%test "subtyping_void_eq" = test_subtyping (t_void, t_void) (Ok ())

let%test "subtyping_void_ref" =
  test_subtyping (t_void, t_int) (Error [ BadSubtyping (t_void, t_int) ])

let%test "subtyping_void_src" =
  test_subtyping (t_int, t_void) (Error [ BadSubtyping (t_int, t_void) ])

(* ========== Int Type ========== *)

let%test "subtyping_int_eq" = test_subtyping (t_int, t_int) (Ok ())

let%test "subtyping_int_ref" =
  test_subtyping (t_int, t_null) (Error [ BadSubtyping (t_int, t_null) ])

let%test "subtyping_int_src" =
  test_subtyping (t_null, t_int) (Error [ BadSubtyping (t_null, t_int) ])

let%test "subtyping_int_literal" = test_subtyping (t_int, lt_integer 10) (Ok ())

(* ========== Float Type ========== *)

let%test "subtyping_float_eq" = test_subtyping (t_float, t_float) (Ok ())

let%test "subtyping_float_ref" =
  test_subtyping (t_float, t_null) (Error [ BadSubtyping (t_float, t_null) ])

let%test "subtyping_float_src" =
  test_subtyping (t_null, t_float) (Error [ BadSubtyping (t_null, t_float) ])

let%test "subtyping_float_literal" =
  test_subtyping (t_float, lt_float 10.1) (Ok ())

(* ========== String Type ========== *)

let%test "subtyping_string_eq" = test_subtyping (t_string, t_string) (Ok ())

let%test "subtyping_string_ref" =
  test_subtyping (t_string, t_null) (Error [ BadSubtyping (t_string, t_null) ])

let%test "subtyping_string_src" =
  test_subtyping (t_null, t_string) (Error [ BadSubtyping (t_null, t_string) ])

let%test "subtyping_string_literal" =
  test_subtyping (t_string, lt_string "abc") (Ok ())

(* ========== Boolean Type ========== *)

let%test "subtyping_boolean_eq" = test_subtyping (t_boolean, t_boolean) (Ok ())

let%test "subtyping_boolean_ref" =
  test_subtyping (t_boolean, t_null) (Error [ BadSubtyping (t_boolean, t_null) ])

let%test "subtyping_boolean_src" =
  test_subtyping (t_null, t_boolean) (Error [ BadSubtyping (t_null, t_boolean) ])

let%test "subtyping_boolean_literal" =
  test_subtyping (t_boolean, lt_boolean true) (Ok ())

(* ========== Symbol Type ========== *)

let%test "subtyping_symbol_eq" = test_subtyping (t_symbol, t_symbol) (Ok ())

let%test "subtyping_symbol_ref" =
  test_subtyping (t_symbol, t_null) (Error [ BadSubtyping (t_symbol, t_null) ])

let%test "subtyping_symbol_src" =
  test_subtyping (t_null, t_symbol) (Error [ BadSubtyping (t_null, t_symbol) ])

let%test "subtyping_symbol_literal" =
  test_subtyping (t_symbol, lt_symbol "a") (Ok ())

(* ========== Literal Type ========== *)

let%test "subtyping_literal_eq" =
  test_subtyping (lt_integer 10, lt_integer 10) (Ok ())

let%test "subtyping_literal_badval" =
  test_subtyping
    (lt_integer 10, lt_integer 20)
    (Error [ BadSubtyping (lt_integer 10, lt_integer 20) ])

let%test "subtyping_literal_badtype" =
  test_subtyping
    (lt_integer 10, lt_string "abc")
    (Error [ BadSubtyping (lt_integer 10, lt_string "abc") ])

(* ========== Object Types (Literal) ========== *)

let%test "subtyping_literal_object_empty" =
  let otref = t_obj [] in
  let otsrc = t_obj [] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_one_field" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_obj [ t_fld "foo" t_int ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_multiple_fields" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_innerobj_field" =
  let otref_inner = t_obj [ t_fld "baz" t_string ] in
  let otsrc_inner = t_obj [ t_fld "baz" t_string ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" otref_inner ] in
  let otsrc = t_obj [ t_fld "foo" t_int; t_fld "bar" otsrc_inner ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_missing_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int ] in
  test_subtyping (otref, otsrc)
    (Error [ BadSubtyping (otref, otsrc); MissingField ~@"bar" ])

let%test "subtyping_literal_object_extra_field" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_subtyping (otref, otsrc)
    (Error [ BadSubtyping (otref, otsrc); ExtraField ~@"bar" ])

let%test "subtyping_literal_object_incompatible_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "bar" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadSubtyping (t_string, t_boolean)
       ] )

let%test "subtyping_literal_object_incompatible_innerobj_field" =
  let otref_inner = t_obj [ t_fld "baz" t_string ] in
  let otsrc_inner = t_objlit [ t_fld "baz" t_boolean ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" otref_inner ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "bar" otsrc_inner ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadSubtyping (otref_inner, otsrc_inner)
       ; IncompatibleField ~@"baz"
       ; BadSubtyping (t_string, t_boolean)
       ] )

let%test "subtyping_literal_object_covariant_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_unknown ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_optional_field_eq" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_optional_field_ref" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_optional_field_src" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadSubtyping (t_string, t_union [ t_string; t_undefined ])
       ; BadSubtyping (t_string, t_undefined)
       ] )

let%test "subtyping_literal_object_optional_field_covariant" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_unknown ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_boolean ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_optional_field_different" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadSubtyping (t_string, t_boolean)
       ] )

let%test "subtyping_literal_object_optional_field_undefined" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "bar" t_undefined ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_optional_field_missing" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_optional_field_incompatible" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "bar" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadSubtyping (t_union [ t_string; t_undefined ], t_boolean)
       ] )

let%test "subtyping_literal_object_optional_field_union" =
  let union = t_union [ t_string; t_undefined ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "bar" union ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_summary_field_eq" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_summary_field_ref" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_summary_field_src" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_summary_field_covariant" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_unknown ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_boolean ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_summary_field_different" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"*"
       ; BadSubtyping (t_string, t_boolean)
       ] )

let%test "subtyping_literal_object_summary_field_compatible" =
  let extra = t_fld "bar" t_string in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_summary_field_undefined" =
  let extra = t_fld "bar" t_undefined in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_summary_field_incompatible" =
  let extra = t_fld "bar" t_boolean in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleSummaryField ~@"bar"
       ; BadSubtyping (t_union [ t_string; t_undefined ], t_boolean)
       ] )

let%test "subtyping_literal_object_summary_field_union" =
  let extra = t_fld "bar" (t_union [ t_string; t_undefined ]) in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_literal_object_summary_field_option" =
  let extra = t_fld "bar" ~opt:true t_string in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objlit [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc) (Ok ())

(* ========== Object Type (Stored) ========== *)

let%test "subtyping_stored_object_empty" =
  let otref = t_obj [] in
  let otsrc = t_objsto [] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_one_field" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_objsto [ t_fld "foo" t_int ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_multiple_fields" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_innerobj_field" =
  let otref_inner = t_obj [ t_fld "baz" t_string ] in
  let otsrc_inner = t_objsto [ t_fld "baz" t_string ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" otref_inner ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" otsrc_inner ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_missing_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int ] in
  test_subtyping (otref, otsrc)
    (Error [ BadSubtyping (otref, otsrc); MissingField ~@"bar" ])

let%test "subtyping_stored_object_extra_field" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_incompatible_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "subtyping_stored_object_incompatible_innerobj_field" =
  let otref_inner = t_obj [ t_fld "baz" t_string ] in
  let otsrc_inner = t_objsto [ t_fld "baz" t_boolean ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" otref_inner ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" otsrc_inner ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadSubtyping (otref_inner, otsrc_inner)
       ; IncompatibleField ~@"baz"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "subtyping_stored_object_covariant_field" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_unknown ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_unknown, t_string)
       ] )

let%test "subtyping_stored_object_covariant_object_field" =
  let otref_inner = t_obj [ t_fld "baz" t_string ] in
  let otsrc_inner = t_objsto [ t_fld "baz" t_string; t_fld "qux" t_boolean ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" otref_inner ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" otsrc_inner ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_optional_field_eq" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_optional_field_ref" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_string)
       ] )

let%test "subtyping_stored_object_optional_field_src" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadCongruency (t_string, t_union [ t_string; t_undefined ])
       ] )

let%test "subtyping_stored_object_optional_field_covariant" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_unknown ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_unknown, t_boolean)
       ] )

let%test "subtyping_stored_object_optional_field_different" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "subtyping_stored_object_optional_field_undefined" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" t_undefined ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_undefined)
       ] )

let%test "subtyping_stored_object_optional_field_missing" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int ] in
  test_subtyping (otref, otsrc)
    (Error [ BadSubtyping (otref, otsrc); MissingField ~@"bar" ])

let%test "subtyping_stored_object_optional_field_incompatible" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleOptionalField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_boolean)
       ] )

let%test "subtyping_stored_object_optional_field_union" =
  let union = t_union [ t_string; t_undefined ] in
  let otref = t_obj [ t_fld "foo" t_int; t_fld ~opt:true "bar" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "bar" union ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_summary_field_eq" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_summary_field_ref" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int ] in
  test_subtyping (otref, otsrc)
    (Error [ BadSubtyping (otref, otsrc); MissingSummaryField t_string ])

let%test "subtyping_stored_object_summary_field_src" =
  let otref = t_obj [ t_fld "foo" t_int ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_string ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_summary_field_covariant" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_unknown ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"*"
       ; BadCongruency (t_unknown, t_boolean)
       ] )

let%test "subtyping_stored_object_summary_field_different" =
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_boolean ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleField ~@"*"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "subtyping_stored_object_summary_field_compatible" =
  let extra = t_fld "bar" t_string in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleSummaryField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_string)
       ] )

let%test "subtyping_stored_object_summary_field_undefined" =
  let extra = t_fld "bar" t_undefined in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleSummaryField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_undefined)
       ] )

let%test "subtyping_stored_object_summary_field_incompatible" =
  let extra = t_fld "bar" t_boolean in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc)
    (Error
       [ BadSubtyping (otref, otsrc)
       ; IncompatibleSummaryField ~@"bar"
       ; BadCongruency (t_union [ t_string; t_undefined ], t_boolean)
       ] )

let%test "subtyping_stored_object_summary_field_union" =
  let extra = t_fld "bar" (t_union [ t_string; t_undefined ]) in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc) (Ok ())

let%test "subtyping_stored_object_summary_field_option" =
  let extra = t_fld "bar" ~opt:true t_string in
  let otref = t_obj [ t_fld "foo" t_int; t_fld "*" t_string ] in
  let otsrc = t_objsto [ t_fld "foo" t_int; t_fld "*" t_string; extra ] in
  test_subtyping (otref, otsrc) (Ok ())

(* ========== List Type ========== *)

let%test "subtyping_list_eq" =
  let tref = t_list t_int in
  let tsrc = t_list t_int in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_list_incompatible" =
  let tref = t_list t_int in
  let tsrc = t_list t_null in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); BadSubtyping (t_int, t_null) ])

let%test "subtyping_list_congruency" =
  let tref = t_list t_unknown in
  let tsrc = t_list t_int in
  test_subtyping (tref, tsrc) (Ok ())

(* ========== Tuple Type ========== *)

let%test "subtyping_tuple_eq" =
  let tref = t_tuple [ t_int; t_string ] in
  let tsrc = t_tuple [ t_int; t_string ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_tuple_incompatible" =
  let tref = t_tuple [ t_int; t_string ] in
  let tsrc = t_tuple [ t_int; t_boolean ] in
  test_subtyping (tref, tsrc)
    (Error
       [ BadSubtyping (tref, tsrc)
       ; IncompatibleElement 2
       ; BadSubtyping (t_string, t_boolean)
       ] )

let%test "subtyping_tuple_congruency" =
  let tref = t_tuple [ t_int; t_unknown ] in
  let tsrc = t_tuple [ t_int; t_string ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_tuple_missing" =
  let tref = t_tuple [ t_int; t_string; t_boolean ] in
  let tsrc = t_tuple [ t_int; t_string ] in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); NExpectedElements (3, 2) ])

let%test "subtyping_tuple_extra" =
  let tref = t_tuple [ t_int; t_string ] in
  let tsrc = t_tuple [ t_int; t_string; t_boolean ] in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); NExpectedElements (2, 3) ])

(* ========== Union Type ========== *)

let%test "subtyping_union_eq" =
  let tref = t_union [ t_int; t_string ] in
  let tsrc = t_union [ t_int; t_string ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_union_order" =
  let tref = t_union [ t_int; t_string ] in
  let tsrc = t_union [ t_string; t_int ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_union_extra_ref" =
  let tref = t_union [ t_int; t_string; t_boolean ] in
  let tsrc = t_union [ t_int; t_string ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_union_extra_src" =
  let tref = t_union [ t_int; t_string ] in
  let tsrc = t_union [ t_int; t_string; t_boolean ] in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); BadSubtyping (tref, t_boolean) ])

let%test "subtyping_union_incompatible" =
  let tref = t_union [ t_int; t_string ] in
  let tsrc = t_union [ t_int; t_boolean ] in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); BadSubtyping (tref, t_boolean) ])

let%test "subtyping_union_congruency" =
  let tref = t_union [ t_int; t_unknown ] in
  let tsrc = t_union [ t_int; t_string; t_boolean ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_union_any_ref" =
  let tref = t_union [ t_int; t_any; t_null ] in
  let tsrc = t_union [ t_int; t_string; t_boolean ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_union_any_src" =
  let tref = t_union [ t_int; t_string; t_boolean ] in
  let tsrc = t_union [ t_int; t_any; t_null ] in
  test_subtyping (tref, tsrc) (Ok ())

(* ========== Sigma Types ========== *)

let%test "subtyping_sigma_one_case" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo ] in
  let otsrc = t_obj [ t_fld "type" foo ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_sigma "type" [ otsrc ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_two_cases" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otsrc_foo = t_obj [ t_fld "type" foo ] in
  let otref_bar = t_obj [ t_fld "type" bar ] in
  let otsrc_bar = t_obj [ t_fld "type" bar ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_two_cases_order" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otref_bar = t_obj [ t_fld "type" bar ] in
  let otsrc_foo = t_obj [ t_fld "type" foo ] in
  let otsrc_bar = t_obj [ t_fld "type" bar ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_bar; otsrc_foo ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_complex" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_string ] in
  let otsrc_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otsrc_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_string ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_incompatible_discriminants" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo ] in
  let otsrc = t_obj [ t_fld "disc" foo ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_sigma "disc" [ otsrc ] in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); IncompatibleSigmaDiscriminant ])

let%test "subtyping_sigma_missing_case" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otref_bar = t_obj [ t_fld "type" bar ] in
  let otsrc_foo = t_obj [ t_fld "type" foo ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_extra_case" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otsrc_foo = t_obj [ t_fld "type" foo ] in
  let otsrc_bar = t_obj [ t_fld "type" bar ] in
  let tref = t_sigma "type" [ otref_foo ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); ExtraSigmaCase bar ])

let%test "subtyping_sigma_incompatible_case" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_string ] in
  let otsrc_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otsrc_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_boolean ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_subtyping (tref, tsrc)
    (Error
       [ BadSubtyping (tref, tsrc)
       ; IncompatibleSigmaCase bar
       ; BadSubtyping (otref_bar, otsrc_bar)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_string, t_boolean)
       ] )

let%test "subtyping_sigma_covariant_case_literal" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_objlit [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_objlit [ t_fld "type" bar; t_fld "bar" t_unknown ] in
  let otsrc_foo = t_objlit [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otsrc_bar = t_objlit [ t_fld "type" bar; t_fld "bar" t_string ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_covariant_case_stored" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_objsto [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_objsto [ t_fld "type" bar; t_fld "bar" t_unknown ] in
  let otsrc_foo = t_objsto [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otsrc_bar = t_objsto [ t_fld "type" bar; t_fld "bar" t_string ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_sigma "type" [ otsrc_foo; otsrc_bar ] in
  test_subtyping (tref, tsrc)
    (Error
       [ BadSubtyping (tref, tsrc)
       ; IncompatibleSigmaCase bar
       ; BadSubtyping (otref_bar, otsrc_bar)
       ; IncompatibleField ~@"bar"
       ; BadCongruency (t_unknown, t_string)
       ] )

(* ========== Sigma Unfolding ========== *)

let%test "subtyping_sigma_one_case_unfolding" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_obj [ t_fld "type" foo ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_two_cases_unfolding" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otref_bar = t_obj [ t_fld "type" bar ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_obj [ t_fld "type" foo ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_second_unfolding" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo ] in
  let otref_bar = t_obj [ t_fld "type" bar ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_obj [ t_fld "type" bar ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_complex_unfolding" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref_foo = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let otref_bar = t_obj [ t_fld "type" bar; t_fld "bar" t_string ] in
  let tref = t_sigma "type" [ otref_foo; otref_bar ] in
  let tsrc = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_missing_discriminant_unfolding" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_obj [ t_fld "dsc" foo ] in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); MissingSigmaCaseDiscriminant ~@"type" ])

let%test "subtyping_sigma_unknown_discriminant_unfolding" =
  let (foo, bar) = (lt_string "foo", lt_string "bar") in
  let otref = t_obj [ t_fld "type" foo ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_obj [ t_fld "type" bar ] in
  test_subtyping (tref, tsrc)
    (Error [ BadSubtyping (tref, tsrc); UnknownSigmaCaseDiscriminant bar ])

let%test "subtyping_sigma_incompatible_unfolding" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo; t_fld "foo" t_int ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_obj [ t_fld "type" foo; t_fld "foo" t_string ] in
  test_subtyping (tref, tsrc)
    (Error
       [ BadSubtyping (tref, tsrc)
       ; IncompatibleSigmaCase foo
       ; BadSubtyping (otref, tsrc)
       ; IncompatibleField ~@"foo"
       ; BadCongruency (t_int, t_string)
       ] )

let%test "subtyping_sigma_covariant_literal_unfolding" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo; t_fld "foo" t_unknown ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_objlit [ t_fld "type" foo; t_fld "foo" t_int ] in
  test_subtyping (tref, tsrc) (Ok ())

let%test "subtyping_sigma_covariant_stored_unfolding" =
  let foo = lt_string "foo" in
  let otref = t_obj [ t_fld "type" foo; t_fld "foo" t_unknown ] in
  let tref = t_sigma "type" [ otref ] in
  let tsrc = t_objsto [ t_fld "type" foo; t_fld "foo" t_int ] in
  test_subtyping (tref, tsrc)
    (Error
       [ BadSubtyping (tref, tsrc)
       ; IncompatibleSigmaCase foo
       ; BadSubtyping (otref, tsrc)
       ; IncompatibleField ~@"foo"
       ; BadCongruency (t_unknown, t_int)
       ] )
