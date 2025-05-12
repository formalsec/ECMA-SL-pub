/**
 * * javascript/regex/regex_28.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /\BLO/;
let ret = regex.exec("HELLO, LOOK AT YOU");

AssertEquals(ret[0], "LO");
AssertEquals(ret.index, 3);