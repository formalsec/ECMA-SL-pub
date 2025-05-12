/**
 * * javascript/regex/regex_1.js
 * 
 * Simple regex test: no description
*/

let regex = /ab/;
let ret = regex.exec("ccabcc");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 1);
AssertEquals(ret[0], "ab");
AssertEquals(ret.index, 2);
AssertEquals(ret.input, "ccabcc");
