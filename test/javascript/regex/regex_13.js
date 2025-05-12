/**
 * * javascript/regex/regex_13.js
 * 
 * Simple regex test: no description
*/

let regex = /aaa(?=aa)/;
let ret = regex.exec("aaa");

AssertEquals(ret, null);
