/**
 * * javascript/regex/regex_2.js
 * 
 * Simple regex test: disjunction
*/

let regex = /a|b/;
let ret = regex.exec("ccbacc");

AssertEquals(ret[0], "b");
AssertEquals(ret.index, 2);
