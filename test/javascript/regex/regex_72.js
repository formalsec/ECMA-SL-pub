/**
 * * javascript/regex/regex_72.js
 * 
 * Simple regex test: no description
*/

AssertEquals("\xFF", "\u00FF");

let arr = /\xFF/.exec("\u00FF");
AssertEquals(arr[0], "\u00FF");

arr = /\u00FF/.exec("\xFF");
AssertEquals(arr[0], "\u00FF");

arr = /\u00FF/.exec("\u00FF");
AssertEquals(arr[0], "\u00FF");

arr = /[\xFF]/.exec("\u00FF");
AssertEquals(arr[0], "\u00FF");

arr = /[\u00FF]/.exec("\xFF");
AssertEquals(arr[0], "\u00FF");

let ret = String.fromCharCode(255);
AssertEquals(ret, "\u00FF");
AssertEquals(ret, "\xFF");
