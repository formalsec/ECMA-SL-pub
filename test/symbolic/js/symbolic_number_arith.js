const esl_symbolic_ffi = require("esl_symbolic");

let x = esl_symbolic_ffi.number("x");
let y = esl_symbolic_ffi.number("y");

let result = x + y;

esl_symbolic_ffi.assert(result == x + y);
esl_symbolic_ffi.assert(x * 2 == 2 * x);
esl_symbolic_ffi.assert(x - y + y == x);
