/**
 * * javascript/regex/regex_12.js
 * 
 * Simple regex test: repeat-matcher
*/

let regex = /c{2}/gim;
let ret = regex.exec("ccaaaaacc");

AssertEquals(regex.lastIndex, 2);
AssertEquals(ret[0], "cc");
AssertEquals(ret.index, 0);
