const esl_symbolic_ffi = require("esl_symbolic");

let x = esl_symbolic_ffi.number("x");

esl_symbolic_ffi.assert(x !== undefined);
esl_symbolic_ffi.assert(typeof x === "number");
esl_symbolic_ffi.assert(x + 2 == 2 + x);
