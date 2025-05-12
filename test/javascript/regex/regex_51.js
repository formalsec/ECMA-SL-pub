/**
 * * javascript/regex/regex_51.js
 * 
 * Simple regex test: no description
*/

let regex = new RegExp("a|b");
let ret = regex.exec("a");

AssertEquals(ret[0], "a");
AssertEquals(ret.index, 0);
