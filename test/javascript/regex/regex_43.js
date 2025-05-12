/**
 * * javascript/regex/regex_43.js
 * 
 * Simple regex test: no description
*/

let regex = /\n\v\f\r\t\0/;
let ret = regex.exec("ccab\n\v\f\r\t\0cc");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 1);
AssertEquals(ret[0], "\n\v\f\r\t\0");
AssertEquals(ret.index, 4);
AssertEquals(ret.input, "ccab\n\v\f\r\t\0cc");

regex = /[\n]+/;
ret = regex.exec("ccab\n\n\ncc");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 1);
AssertEquals(ret[0], "\n\n\n");
AssertEquals(ret.index, 4);
AssertEquals(ret.input, "ccab\n\n\ncc");