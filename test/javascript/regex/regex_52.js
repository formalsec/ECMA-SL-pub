/**
 * * javascript/regex/regex_52.js
 * 
 * Simple regex test: no description
*/

let regex = new RegExp("\\n+", "gim");
let ret = regex.exec("\n\n");

AssertEquals(ret[0], "\n\n");
AssertEquals(ret.index, 0);
