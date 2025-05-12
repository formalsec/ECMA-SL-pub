/**
 * * javascript/regex/regex_22.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a$/m;
let ret = regex.exec("a");

AssertEquals(ret[0], "a");
