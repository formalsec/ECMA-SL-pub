/**
 * * javascript/regex/regex_25.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a$/m;
let ret = regex.exec("c\na\nc");

AssertEquals(ret[0], "a");