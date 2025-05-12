open Ecma_sl
open Test

(* ========== Value Expression ========== *)

let%test "value_null" = TypeExpr.test ~@(EExpr.Val Value.Nothing) (Ok t_null)

let%test "value_integer" =
  TypeExpr.test ~@(EExpr.Val (Value.Int 10)) (Ok (lt_integer 10))

let%test "value_float" =
  TypeExpr.test ~@(EExpr.Val (Value.Real 10.1)) (Ok (lt_float 10.1))

let%test "value_string" =
  TypeExpr.test ~@(EExpr.Val (Value.Str "abc")) (Ok (lt_string "abc"))

let%test "value_boolean" =
  TypeExpr.test ~@(EExpr.Val Value.True) (Ok (lt_boolean true))

let%test "value_symbol" =
  TypeExpr.test
    ~@(EExpr.Val (Value.App (`Op "symbol", [ Str "a" ])))
    (Ok (lt_symbol "a"))

(* ========== Variable Expressions ========== *)

let%test "variable_integer" =
  let tctx = TypeExpr.tctx [ (~@"foo", t_int) ] in
  TypeExpr.test ~tctx ~@(EExpr.Var "foo") (Ok t_int)

let%test "variable_string" =
  let tctx = TypeExpr.tctx [ (~@"foo", t_string) ] in
  TypeExpr.test ~tctx ~@(EExpr.Var "foo") (Ok t_string)

let%test "variable_missing" =
  TypeExpr.test ~@(EExpr.Var "foo") (Error [ UnknownVar "foo" ])

(* ========== Generic Operator Call ========== *)

let%test "operator_call_ok" =
  let op = ~@EExpr.(BinOpt (Plus, ~@(Val (Int 10)), ~@(Val (Int 10)))) in
  TypeExpr.test op (Ok t_int)

let%test "operator_call_sigcase" =
  let op = ~@EExpr.(BinOpt (Plus, ~@(Val (Real 10.1)), ~@(Val (Real 10.1)))) in
  TypeExpr.test op (Ok t_float)

let%test "operator_call_incompatible_first" =
  let op = ~@EExpr.(BinOpt (Plus, ~@(Val (Str "abc")), ~@(Val (Int 10)))) in
  TypeExpr.test op (Error [ BadOperand (t_int, lt_string "abc") ])

let%test "operator_call_incompatible_second" =
  let op = ~@EExpr.(BinOpt (Plus, ~@(Val (Int 10)), ~@(Val (Real 10.1)))) in
  TypeExpr.test op (Error [ BadOperand (t_int, lt_float 10.1) ])

(* ========== Object Expression  ========== *)

let%test "object_empty" = TypeExpr.test ~@EExpr.(NewObj []) (Ok (t_objlit []))

let%test "object_one_field" =
  let foo = (~@"foo", ~@EExpr.(Val (Int 10))) in
  let tfoo = t_fld "foo" (lt_integer 10) in
  TypeExpr.test ~@EExpr.(NewObj [ foo ]) (Ok (t_objlit [ tfoo ]))

let%test "object_multiple_fields" =
  let foo = (~@"foo", ~@EExpr.(Val (Int 10))) in
  let bar = (~@"bar", ~@EExpr.(Val (Str "abc"))) in
  let tfoo = t_fld "foo" (lt_integer 10) in
  let tbar = t_fld "bar" (lt_string "abc") in
  TypeExpr.test ~@EExpr.(NewObj [ foo; bar ]) (Ok (t_objlit [ tfoo; tbar ]))

let%test "object_innerobj_field" =
  let foo = (~@"foo", ~@EExpr.(Val (Int 10))) in
  let bar = (~@"bar", ~@EExpr.(NewObj [ (~@"baz", ~@(Val (Str "abc"))) ])) in
  let tfoo = t_fld "foo" (lt_integer 10) in
  let tbar = t_fld "bar" (t_objlit [ t_fld "baz" (lt_string "abc") ]) in
  TypeExpr.test ~@EExpr.(NewObj [ foo; bar ]) (Ok (t_objlit [ tfoo; tbar ]))
