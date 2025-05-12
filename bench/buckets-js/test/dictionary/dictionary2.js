// --------------------------------- tests -----------------------------------
const buckets = require("./../../src/dictionary");
const esl_symbolic = require("esl_symbolic");

var dict = new buckets.Dictionary();

var x1 = esl_symbolic.number("x1"); //1
var x2 = esl_symbolic.number("x2"); //2
var s1 = esl_symbolic.string("s1"); // "2"
var s2 = esl_symbolic.string("s2"); // "foo"

dict.set(s1, x1);
dict.set(s2, x2);

var res1 = dict.remove(s1);
esl_symbolic.assert(((s1 == s2) && (res1 == x2)) || ((!(s1 == s2)) && (res1 == x1)));
var res2 = dict.remove(s1);
esl_symbolic.assert(res2 == undefined);
