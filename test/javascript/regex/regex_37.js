/**
 * * javascript/regex/regex_37.js
 * 
 * Simple regex test: non-capturing group
*/

let regex = /[abc]/;
let ret = regex.exec("ccaabb");

AssertEquals(ret[0], "c");
AssertEquals(ret.index, 0);
AssertEquals(ret.length, 1);