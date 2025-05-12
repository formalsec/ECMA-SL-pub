/**
 * * javascript/regex/regex_33.js
 * 
 * Simple regex test: non-capturing group
*/

let regex = /c+?a+c+/;
let ret = regex.exec("ccaacc");

AssertEquals(ret[0], ret.input);
AssertEquals(ret.index, 0);
AssertEquals(ret.length, 1);