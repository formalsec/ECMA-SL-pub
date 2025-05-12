/**
 * * javascript/regex/regex_17.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a/;
let ret = regex.exec("ab");

AssertEquals(ret[0], "a");
