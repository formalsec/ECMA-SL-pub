/**
 * * javascript/regex/regex_8.js
 * 
 * Simple regex test: repeat-matcher
*/

let regex = /a{1,3}/;
let ret = regex.exec("ccaaaaacc");

AssertEquals(ret[0], "aaa");
AssertEquals(ret.index, 2);
