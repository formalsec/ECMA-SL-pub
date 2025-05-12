/**
 * * javascript/regex/regex_64.js
 * 
 * Simple regex test: no description
*/

let x;
let regex = new RegExp(x, "g");
let expected = /(?:)/g;
AssertEquals(regex.source, expected.source);

let str = String("asdf");
AssertEquals(str, "asdf");

let replaced = str.replace(regex, "1");
AssertEquals(replaced, "1a1s1d1f1");
