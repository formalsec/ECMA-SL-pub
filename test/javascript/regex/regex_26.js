/**
 * * javascript/regex/regex_26.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /\bLO/;
let ret = regex.exec("HELLO, LOOK AT YOU");

AssertEquals(ret[0], "LO");
AssertEquals(ret.index, 7);