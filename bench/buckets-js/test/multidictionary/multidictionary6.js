// ---------------------------------- tests -----------------------------------
const buckets = require("../../src/multidictionary");
const esl_symbolic = require("esl_symbolic");

var dict1 = new buckets.MultiDictionary();
var dict2 = new buckets.MultiDictionary();

var s1 = esl_symbolic.string("s1");
var s2 = esl_symbolic.string("s2");
var x1 = esl_symbolic.number("x1");
var x2 = esl_symbolic.number("x2");

esl_symbolic.assume (!(x1 == x2));

dict1.set(s1, x1);
dict1.set(s1, x2);

var res1 = dict1.equals(dict2);
esl_symbolic.assert(!res1);
var res2 = dict1.equals(dict1.keys());
esl_symbolic.assert(!res2);

dict2.set(s2, x1);
dict2.set(s2, x2);

var res3 = dict1.equals(dict2);
esl_symbolic.assert(((s1 == s2) && res3) || ((!(s1 == s2)) && (!res3)));
