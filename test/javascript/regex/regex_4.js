/**
 * * javascript/regex/regex_4.js
 * 
 * Simple regex test: no description
*/

let regex = /(a|b)\1/;
let ret = regex.exec("ccbbcc");

AssertEquals(ret[0], "bb");
AssertEquals(ret[1], "b");
AssertEquals(ret.index, 2);
AssertEquals(ret.length, 2);
