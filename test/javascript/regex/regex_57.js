/**
 * * javascript/regex/regex_57.js
 * 
 * Simple regex test: no description
*/

let ret = "abcA".replace(/a/ig, "yo");
AssertEquals(ret, 'yobcyo');

ret = "abcA".replace(/a/ig, "d");
AssertEquals(ret, 'dbcd');
