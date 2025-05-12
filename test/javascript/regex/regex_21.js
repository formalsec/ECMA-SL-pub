/**
 * * javascript/regex/regex_21.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a$/;
let ret = regex.exec("a");

AssertEquals(ret[0], "a");
