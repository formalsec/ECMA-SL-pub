/**
 * * javascript/regex/regex_47.js
 * 
 * Simple regex test: no description
*/

let regex = /\d\D/;

let ret = regex.exec("0a");
AssertEquals(ret[0], "0a");

ret = regex.exec("\t a\t");
AssertEquals(ret, null);