/**
 * * javascript/regex/regex_34.js
 * 
 * Simple regex test: non-capturing group
*/

let regex = /a*?/;
let ret = regex.exec("aaa");

AssertEquals(ret[0], "");
AssertEquals(ret.index, 0);
AssertEquals(ret.length, 1);