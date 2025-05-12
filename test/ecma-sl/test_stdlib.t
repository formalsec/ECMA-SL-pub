Tests include:
  $ ecma-sl interpret test_stdlib_import.esl --exitval
  [ecma-sl] exit value: 0
  $ ecma-sl interpret test_stdlib_float.esl --exitval
  [ecma-sl] exit value: 0
Tests list operators:
  $ ecma-sl interpret test_stdlib_list.esl --exitval
  Initial list:
  8
  4
  2
  0
  7
  1
  9
  3
  6
  5
  Squared list:
  64
  16
  4
  0
  49
  1
  81
  9
  36
  25
  Sum of squares: 285
  Sorted list:
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  [ecma-sl] exit value: 0
  $ ecma-sl interpret test_stdlib_inequality.esl --exitval
  [ecma-sl] exit value: 0
  $ ecma-sl interpret test_stdlib_fpath.esl --exitval
  [ecma-sl] exit value: 0
  $ ecma-sl interpret test_stdlib_queue.esl --exitval
  [ecma-sl] exit value: 0
