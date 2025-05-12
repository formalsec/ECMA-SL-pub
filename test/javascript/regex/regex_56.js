/**
 * * javascript/regex/regex_56.js
 * 
 * Simple regex test: no description
*/

let ret = "abc".split(/a/ig);
AssertEquals(ret[0], '')
AssertEquals(ret[1], 'bc')

ret = "abc".split("b");
AssertEquals(ret[0], 'a')
AssertEquals(ret[1], 'c')
