/**
 * * javascript/regex/regex_55.js
 * 
 * Simple regex test: no description
*/

let ret = "abc".search(/b/ig);
AssertEquals(ret, 1);

ret = "aaa".search(/b/ig);
AssertEquals(ret, -1);

let str = "Mr. Blue has a blue house"
ret = str.search("blue");
AssertEquals(ret, 15);