/**
 * * javascript/regex/regex_61.js
 * 
 * Simple regex test: no description
*/

let regex = new RegExp(/\n+/gim);
let expected = /\n+/gim;

AssertEquals(regex.source, expected.source);
