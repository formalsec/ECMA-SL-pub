/**
 * * javascript/regex/regex_39.js
 * 
 * Simple regex test: no description
*/

let regex = /.+/;
let ret = regex.exec("ccabcc");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 1);
AssertEquals(ret[0], "ccabcc");
AssertEquals(ret.index, 0);
AssertEquals(ret.input, "ccabcc");
