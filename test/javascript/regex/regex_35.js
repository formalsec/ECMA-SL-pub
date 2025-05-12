/**
 * * javascript/regex/regex_35.js
 * 
 * Simple regex test: non-capturing group
*/

let regex = /a+?/;
let ret = regex.exec("aaa");

AssertEquals(ret[0], "a");
AssertEquals(ret.index, 0);
AssertEquals(ret.length, 1);