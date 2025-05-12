/**
 * * javascript/regex/regex_3.js
 * 
 * Simple regex test: back-reference
*/

let regex = /(ab)\1/;
let ret = regex.exec("ccababcc");

AssertEquals(ret[0], "abab");
AssertEquals(ret.index, 2);
