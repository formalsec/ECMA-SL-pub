/**
 * * javascript/regex/regex_9.js
 * 
 * Simple regex test: repeat-matcher
*/

let regex = /a{1,}/;
let ret = regex.exec("ccaaaaacc");

AssertEquals(ret[0], "aaaaa");
AssertEquals(ret.index, 2);
