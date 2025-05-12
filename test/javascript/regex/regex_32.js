/**
 * * javascript/regex/regex_32.js
 * 
 * Simple regex test: non-capturing group
*/

let regex = /a*?/;
let ret = regex.exec("ccaacc");

AssertEquals(ret[0], "");
AssertEquals(ret.index, 0);
AssertEquals(ret.length, 1);