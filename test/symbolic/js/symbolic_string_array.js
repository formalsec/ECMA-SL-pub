const esl_symbolic_ffi = require("esl_symbolic");

let arr = [esl_symbolic_ffi.string("flour"), esl_symbolic_ffi.string("water")];
let loaf = arr.join(" ");

esl_symbolic_ffi.assert(loaf != "banana bread");
