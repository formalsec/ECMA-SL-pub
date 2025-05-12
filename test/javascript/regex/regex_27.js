/**
 * * javascript/regex/regex_27.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /LO\b/;
let ret = regex.exec("HELLO, LOOK AT YOU");

AssertEquals(ret[0], "LO");
AssertEquals(ret.index, 3);