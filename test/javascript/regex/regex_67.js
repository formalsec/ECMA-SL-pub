/**
 * * javascript/regex/regex_67.js
 * 
 * Simple regex test: no description
*/

let str = 'abcdefcdef';
let regex = /c/g;
let ret = str.replace(regex, "$`" + 'SS');

AssertEquals(ret, 'ababSSdefabcdefSSdef');
