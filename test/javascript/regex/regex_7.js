/**
 * * javascript/regex/regex_7.js
 * 
 * Simple regex test: repeat-matcher
*/

let regex = /a?/;
let ret = regex.exec("aaacc");

AssertEquals(ret[0], "a");
AssertEquals(ret.index, 0);
