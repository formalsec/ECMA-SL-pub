/**
 * * javascript/regex/regex_68.js
 * 
 * Simple regex test: no description
*/

let str = 'uid=31';
let regex = /(uid=)(\d+)/;
let ret = str.replace(regex, "$11" + 15);

AssertEquals(ret, 'uid=115');
