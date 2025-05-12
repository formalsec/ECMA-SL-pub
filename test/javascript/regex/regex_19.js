/**
 * * javascript/regex/regex_19.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a/m;
let ret = regex.exec("ab");

AssertEquals(ret[0], "a");
