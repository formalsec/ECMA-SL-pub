const esl_symbolic_ffi = require("esl_symbolic");

let x = esl_symbolic_ffi.number("x");

if (x > 0) {
  esl_symbolic_ffi.assert(x > 0);
} else {
  esl_symbolic_ffi.assert(x <= 0);
}
