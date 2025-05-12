const esl_symbolic_ffi = require("esl_symbolic");

let str = esl_symbolic_ffi.string("str");

if (str === "saucisse") {
  esl_symbolic_ffi.assert(str === "saucisse");
} else {
  esl_symbolic_ffi.assert(str !== "saucisse");
}
