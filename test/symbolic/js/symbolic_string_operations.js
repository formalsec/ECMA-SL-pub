const esl_symbolic_ffi = require("esl_symbolic");

let str1 = esl_symbolic_ffi.string("str1");
let str2 = esl_symbolic_ffi.string("str2");

let result = str1 + str2;

esl_symbolic_ffi.assert(result == str1 + str2);
esl_symbolic_ffi.assert(str1 + "test" == str1 + "test");
