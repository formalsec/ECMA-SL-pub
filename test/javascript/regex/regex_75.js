/**
 * * javascript/regex/regex_75.js
 * 
 * Simple regex test: no description
*/

let ret = /(z)((a+)?(b+)?(c))*/.exec("zaacbbbcac");

AssertEquals(ret[0], "zaacbbbcac");
AssertEquals(ret[1], "z");
AssertEquals(ret[2], "ac");
AssertEquals(ret[3], "a");
AssertEquals(ret[4], undefined);
AssertEquals(ret[5], "c");
