/**
 * * javascript/regex/regex_70.js
 * 
 * Simple regex test: octal
*/

let str = '\101';
let str2 = 'A';
AssertEquals(str, str2);

let regex = /\101/;
let ret = regex.test("A");
AssertEquals(ret, true);

regex = /[\101]/;
ret = regex.test("A");
AssertEquals(ret, true);
