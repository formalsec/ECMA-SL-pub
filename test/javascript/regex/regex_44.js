/**
 * * javascript/regex/regex_44.js
 * 
 * Simple regex test: no description
*/

let regex = /[.]/;
let ret = regex.exec(".");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 1);
AssertEquals(ret[0], ".");
AssertEquals(ret.index, 0);

ret = regex.exec("m");

AssertEquals(ret, null);