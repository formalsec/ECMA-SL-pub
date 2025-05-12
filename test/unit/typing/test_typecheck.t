Run ecma-sl type checking tests:
  $ find examples/** -name "*.esl" | sort -h | xargs -I{} sh -c 'ecma-sl compile "$1" -o /dev/null; exit 0' -- {}
  TypeError: Value of type '"abc"' cannot be returned by a 'int' function.
  File "return.esl", line 11, characters 45-50
  11 |   function badValueReturn(): int      { return "abc"; }		/* BadReturn: int <- string */
                                                      ^^^^^
  TypeError: Value of type 'void' cannot be returned by a 'int' function.
  TypeError: Value of type '10' cannot be returned by a 'void' function.
  File "return.esl", line 12, characters 45-47
  12 |   function badValueVoidReturn(): void { return 10; }				/* BadReturn: void <- int */
                                                      ^^
