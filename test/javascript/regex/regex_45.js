/**
 * * javascript/regex/regex_45.js
 * 
 * Simple regex test: no description
*/

let regex = /(?=(.*b))(a)(b)/;
let ret = regex.exec("abc");

AssertEquals(regex.lastIndex, 0);
AssertEquals(ret.length, 4);
AssertEquals(ret[0], "ab");
AssertEquals(ret.index, 0);
