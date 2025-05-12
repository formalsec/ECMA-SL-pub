/**
 * * javascript/regex/regex_73.js
 * 
 * Simple regex test: no description
*/

let arr = /\w{3}\d?/.exec("CE\uFFFFL\uFFDDaba12");
AssertEquals(arr[0], "aba1");
