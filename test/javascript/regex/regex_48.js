/**
 * * javascript/regex/regex_48.js
 * 
 * Simple regex test: no description
*/

let regex = /\w+\W+/;

let ret = regex.exec("0azAZ_.,-");
AssertEquals(ret[0], "0azAZ_.,-");

ret = regex.exec("0azAZ");
AssertEquals(ret, null);