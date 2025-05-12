/**
 * * javascript/regex/regex_46.js
 * 
 * Simple regex test: no description
*/

let regex = /\s{2}\S?/;

let ret = regex.exec("\t 0");
AssertEquals(ret[0], "\t 0");

ret = regex.exec("\t a\t");
AssertEquals(ret[0], "\t a");

ret = regex.exec("\t ");
AssertEquals(ret[0], "\t ");

ret = regex.exec("  ");
AssertEquals(ret[0], "  ");

ret = regex.exec(" a");
AssertEquals(ret, null);