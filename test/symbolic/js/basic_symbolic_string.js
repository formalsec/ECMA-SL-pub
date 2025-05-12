const esl_symbolic_ffi = require("esl_symbolic");

let str = esl_symbolic_ffi.string("str");

esl_symbolic_ffi.assert(str !== undefined);
esl_symbolic_ffi.assert(typeof str === "string");
esl_symbolic_ffi.assert(str + "test" == str + "test");
