/**
 * * javascript/regex/regex_49.js
 * 
 * Simple regex test: no description
*/

let regex = /[^a-c]+/;

let ret = regex.exec("abcdef");
AssertEquals(ret[0], "def");

ret = regex.exec("abcabc");
AssertEquals(ret, null);