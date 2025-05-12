/**
 * * javascript/regex/regex_60.js
 * 
 * Simple regex test: no description
*/

let str = new String("test string");
let ret = str.search(/String/i);

AssertEquals(ret, 5);
