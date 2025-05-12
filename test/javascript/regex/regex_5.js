/**
 * * javascript/regex/regex_5.js
 * 
 * Simple regex test: repeat-matcher
*/

let regex = /a+/;
let ret = regex.exec("ccaaacc");

AssertEquals(ret[0], "aaa");
AssertEquals(ret.index, 2);
