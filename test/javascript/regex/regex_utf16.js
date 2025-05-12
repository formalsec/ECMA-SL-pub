/**
 * * javascript/regex/regex_utf16.js
 * 
 * Simple regex test: no description
*/

var regex = /b/;
var res1 = regex.exec('aab').index;
var res2 = regex.exec('😃aab').index;

AssertEquals(res1, 2);
AssertEquals(res2, 4);
AssertEquals("😃".length, 2);
