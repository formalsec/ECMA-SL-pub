/**
 * * javascript/regex/regex_62.js
 * 
 * Simple regex test: no description
*/

let regex = /hh/g;
let str = "hh oo hh";

let ret = regex.exec(str);
AssertEquals(ret.index, 0);

ret = regex.exec(str);
AssertEquals(ret.index, 6);
