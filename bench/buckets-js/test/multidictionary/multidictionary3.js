// ---------------------------------- tests -----------------------------------
const buckets = require("../../src/multidictionary");
const esl_symbolic = require("esl_symbolic");

var dict = new buckets.MultiDictionary()

var s1 = esl_symbolic.string("s1");
var s2 = esl_symbolic.string("s2");
var x1 = esl_symbolic.number("x1");
var x2 = esl_symbolic.number("x2");
var x3 = esl_symbolic.number("x3");

esl_symbolic.assume(!(x1 == x2));
esl_symbolic.assume(!(x1 == x3));
esl_symbolic.assume(!(x2 == x3));

dict.set(s1, x1);
dict.set(s1, x2);
dict.set(s2, x2);
dict.set(s2, x3);

var keys = dict.keys();
var l = keys.length;
esl_symbolic.assert(((s1 == s2) && (l == 1)) || ((!(s1 == s2)) && (l == 2)));

var values = dict.values();
var l2 = values.length;
esl_symbolic.assert(((s1 == s2) && (l2 == 3)) || ((!(s1 == s2)) && (l2 == 4)));
