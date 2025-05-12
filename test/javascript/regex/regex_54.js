/**
 * * javascript/regex/regex_54.js
 * 
 * Simple regex test: no description
*/

let regex = /(b)/ig;
let ret = "abcb".match(regex);

AssertEquals(ret.length, 2);
AssertEquals(ret[0], "b");
AssertEquals(ret[1], "b");