/**
 * * javascript/regex/regex_24.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a$/;
let ret = regex.exec("a\nb");

AssertEquals(ret, null);