/**
 * * javascript/regex/regex_18.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a/;
let ret = regex.exec("b\na");

AssertEquals(ret, null);
