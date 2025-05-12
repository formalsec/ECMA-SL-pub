/**
 * * javascript/regex/regex_11.js
 * 
 * Simple regex test: repeat-matcher
*/

let regex = /c{3,}/;
let ret = regex.exec("ccaaaaacc");

AssertEquals(ret, null);
