open Ecma_sl
open Test

let test_unopt ((op, v) : Operator.unopt * Value.t) (expected : EType.t) : bool
    =
  let v' = ~@(EExpr.Val v) in
  TypeExpr.test ~@(EExpr.UnOpt (op, v')) (Ok expected)

let test_binopt ((op, v1, v2) : Operator.binopt * Value.t * Value.t)
  (expected : EType.t) : bool =
  let (v1', v2') = EExpr.(~@(Val v1), ~@(Val v2)) in
  TypeExpr.test ~@(EExpr.BinOpt (op, v1', v2')) (Ok expected)

let test_triopt
  ((op, v1, v2, v3) : Operator.triopt * Value.t * Value.t * Value.t)
  (expected : EType.t) : bool =
  let (v1', v2', v3') = EExpr.(~@(Val v1), ~@(Val v2), ~@(Val v3)) in
  TypeExpr.test ~@(EExpr.TriOpt (op, v1', v2', v3')) (Ok expected)

(* ========== Simple Unary Operators ========== *)

let%test "unopt_neg_int" = test_unopt (Neg, Int 10) t_int

let%test "unopt_neg_float" = test_unopt (Neg, Real 10.1) t_float

let%test "unopt_bitwise_not" = test_unopt (BitwiseNot, Real 10.1) t_float

let%test "unopt_LogicalNot" = test_unopt (LogicalNot, Value.True) t_boolean

let%test "unopt_int_to_float" = test_unopt (IntToFloat, Int 10) t_float

let%test "unopt_int_to_string" = test_unopt (IntToString, Int 10) t_string

let%test "unopt_float_to_int" = test_unopt (FloatToInt, Real 10.1) t_int

let%test "unopt_float_to_string" =
  test_unopt (FloatToString, Real 10.1) t_string

let%test "unopt_string_to_int" = test_unopt (StringToInt, Str "abc") t_int

let%test "unopt_string_to_float" = test_unopt (StringToFloat, Str "abc") t_float

(* ========== Simple Binary Operators ========== *)

let%test "binopt_plus_int" = test_binopt (Plus, Int 10, Int 10) t_int

let%test "binopt_plus_float" = test_binopt (Plus, Real 10.1, Real 10.1) t_float

let%test "binopt_minus_int" = test_binopt (Minus, Int 10, Int 10) t_int

let%test "binopt_minus_float" =
  test_binopt (Minus, Real 10.1, Real 10.1) t_float

let%test "binopt_times_int" = test_binopt (Times, Int 10, Int 10) t_int

let%test "binopt_times_float" =
  test_binopt (Times, Real 10.1, Real 10.1) t_float

let%test "binopt_div_int" = test_binopt (Div, Int 10, Int 10) t_int

let%test "binopt_div_float" = test_binopt (Div, Real 10.1, Real 10.1) t_float

let%test "binopt_modulo" = test_binopt (Modulo, Real 10.1, Real 10.1) t_float

let%test "binopt_pow" = test_binopt (Pow, Real 10.1, Real 10.1) t_float

let%test "binopt_bitwise_and" = test_binopt (BitwiseAnd, Int 10, Int 10) t_int

let%test "binopt_bitwise_or" = test_binopt (BitwiseOr, Int 10, Int 10) t_int

let%test "binopt_bitwise_xor" = test_binopt (BitwiseXor, Int 10, Int 10) t_int

let%test "binopt_shift_left" = test_binopt (ShiftLeft, Int 10, Int 10) t_int

let%test "binopt_shift_right" = test_binopt (ShiftRight, Int 10, Int 10) t_int

let%test "binopt_shift_right_logical" =
  test_binopt (ShiftRightLogical, Int 10, Int 10) t_int

let%test "binopt_logical_and" =
  test_binopt (LogicalAnd, Value.True, Value.True) t_boolean

let%test "binopt_logical_or" =
  test_binopt (LogicalOr, Value.True, Value.True) t_boolean

let%test "binopt_sc_logical_and" =
  test_binopt (SCLogicalAnd, Value.True, Value.True) t_boolean

let%test "binopt_sc_logical_or" =
  test_binopt (SCLogicalOr, Value.True, Value.True) t_boolean

let%test "binopt_eq_int" = test_binopt (Eq, Int 10, Int 10) t_boolean

let%test "binopt_eq_float" = test_binopt (Eq, Real 10.1, Real 10.1) t_boolean

let%test "binopt_eq_null" =
  test_binopt (Eq, Value.Nothing, Value.Nothing) t_boolean

let%test "binopt_lt_int" = test_binopt (Lt, Int 10, Int 10) t_boolean

let%test "binopt_lt_float" = test_binopt (Lt, Real 10.1, Real 10.1) t_boolean

let%test "binopt_lt_null" =
  test_binopt (Lt, Value.Nothing, Value.Nothing) t_boolean

let%test "binopt_gt_int" = test_binopt (Gt, Int 10, Int 10) t_boolean

let%test "binopt_gt_float" = test_binopt (Gt, Real 10.1, Real 10.1) t_boolean

let%test "binopt_gt_null" =
  test_binopt (Gt, Value.Nothing, Value.Nothing) t_boolean

let%test "binopt_le_int" = test_binopt (Le, Int 10, Int 10) t_boolean

let%test "binopt_le_float" = test_binopt (Le, Real 10.1, Real 10.1) t_boolean

let%test "binopt_ge_null" =
  test_binopt (Ge, Value.Nothing, Value.Nothing) t_boolean

let%test "binopt_le_int" = test_binopt (Le, Int 10, Int 10) t_boolean

let%test "binopt_le_float" = test_binopt (Le, Real 10.1, Real 10.1) t_boolean

let%test "binopt_le_null" =
  test_binopt (Le, Value.Nothing, Value.Nothing) t_boolean
