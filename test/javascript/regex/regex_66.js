/**
 * * javascript/regex/regex_66.js
 * 
 * Simple regex test: no description
*/

let str = 'She sells seashells by the seashore.';
let regex = /sh/g;
let ret = str.replace(regex, "$$" + 'sch');

AssertEquals(ret, 'She sells sea$schells by the sea$schore.');
