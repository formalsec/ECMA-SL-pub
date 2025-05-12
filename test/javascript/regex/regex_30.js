/**
 * * javascript/regex/regex_30.js
 * 
 * Simple regex test: non-capturing group
*/

let regex = /(?:LO)\B/;
let ret = regex.exec("HELLO, LOOK AT YOU");

AssertEquals(ret[0], "LO");
AssertEquals(ret.index, 7);
AssertEquals(ret.length, 1);