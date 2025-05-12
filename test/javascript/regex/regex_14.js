/**
 * * javascript/regex/regex_14.js
 * 
 * Simple regex test: positive look-ahead
*/

let regex = /a*(?=aa)/;
let ret = regex.exec("aaaaa");

AssertEquals(ret[0], "aaa");
