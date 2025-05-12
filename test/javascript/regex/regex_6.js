/**
 * * javascript/regex/regex_6.js
 * 
 * Simple regex test: repeat-matcher
*/

let regex = /a*/;
let ret = regex.exec("aaacc");

AssertEquals(ret[0], "aaa");
AssertEquals(ret.index, 0);
