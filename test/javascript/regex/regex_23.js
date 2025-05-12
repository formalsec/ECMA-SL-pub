/**
 * * javascript/regex/regex_23.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a$/m;
let ret = regex.exec("ab");

AssertEquals(ret, null);