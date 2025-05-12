/**
 * * javascript/regex/regex_74.js
 * 
 * Simple regex test: no description
*/

let str = "easy\bto\u0008ride";
let arr = /[^\b]+/g.exec(str);
AssertEquals(arr[0], "easy");

str = "easy\u0008to\bride";
arr = /[^\b]+/g.exec(str);
AssertEquals(arr[0], "easy");
