/**
 * * javascript/regex/regex_15.js
 * 
 * Simple regex test: positive look-ahead
*/

let regex = /aaa(?!aa)/;
let ret = regex.exec("aaaaa");

AssertEquals(ret[0], "aaa");
AssertEquals(ret.index, 1);
