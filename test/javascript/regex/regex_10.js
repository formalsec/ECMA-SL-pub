/**
 * * javascript/regex/regex_10.js
 * 
 * Simple regex test: repeat-matcher
*/

let regex = /c{2}/;
let ret = regex.exec("ccaaaaacc");

AssertEquals(ret[0], "cc");
AssertEquals(ret.index, 0);
