/**
 * * javascript/regex/regex_29.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /LO\B/;
let ret = regex.exec("HELLO, LOOK AT YOU");

AssertEquals(ret[0], "LO");
AssertEquals(ret.index, 7);