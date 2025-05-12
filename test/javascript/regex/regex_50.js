/**
 * * javascript/regex/regex_50.js
 * 
 * Simple regex test: no description
*/

let regex = /\0+/;

let ret = regex.exec("\0\0");
AssertEquals(ret[0], "\0\0");
AssertEquals(ret.index, 0);

ret = regex.exec("abc\0");
AssertEquals(ret[0], "\0");
AssertEquals(ret.index, 3);