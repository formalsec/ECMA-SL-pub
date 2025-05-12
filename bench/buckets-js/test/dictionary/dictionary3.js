// --------------------------------- tests -----------------------------------
const buckets = require("./../../src/dictionary");
const esl_symbolic = require("esl_symbolic");

var dict = new buckets.Dictionary();

var x1 = esl_symbolic.number("x1"); //1
var x2 = esl_symbolic.number("x2"); //2
var s1 = esl_symbolic.string("s1"); // "2"
var s2 = esl_symbolic.string("s2"); // "foo"

esl_symbolic.assume(!(s1 == s2));

dict.set(s1, x1);
dict.set(s2, x2);

var keys = dict.keys();
var l1 = keys.length;
esl_symbolic.assert(l1 == 2);
var t1 = keys[0];
var t2 = keys[1];
esl_symbolic.assert(((t1 == s1) && (t2 == s2)) || ((t1 == s2) && (t2 == s1)));

var values = dict.values();
var l2 = keys.length;
esl_symbolic.assert(l2 == 2);
var y1 = values[0];
var y2 = values[1];
esl_symbolic.assert(((y1 == x1) && (y2 == x2)) || ((y1 == x2) && (y2 == x1)));
