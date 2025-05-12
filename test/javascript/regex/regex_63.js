/**
 * * javascript/regex/regex_63.js
 * 
 * Simple regex test: no description
*/

let ret = "abcA".replace(/a/ig, "d");

AssertEquals(ret, 'dbcd');
