/**
 * * javascript/regex/regex_20.js
 * 
 * Simple regex test: boundary assertions
*/

let regex = /^a/m;
let ret = regex.exec("cc\nabcc");

AssertEquals(ret[0], "a");
