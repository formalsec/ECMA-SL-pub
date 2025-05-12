Tests relative imports:
  $ ecma-sl compile relative-imports/a.esl
  function a(___internal_esl_global) {
    return [false, "a"]
  };
  function b(___internal_esl_global) {
    return [false, "b"]
  };
  function c(___internal_esl_global) {
    return [false, "c"]
  }
