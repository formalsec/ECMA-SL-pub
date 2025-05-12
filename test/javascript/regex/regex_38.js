/**
 * * javascript/regex/regex_38.js
 * 
 * Simple regex test: non-capturing group
*/

let regex = /[a-z]/;
let ret = regex.exec("Ccaabb");

AssertEquals(ret[0], "c");
AssertEquals(ret.index, 1);
AssertEquals(ret.length, 1);