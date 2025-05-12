// ---------------------------------- tests -----------------------------------
const buckets = require("../../src/multidictionary");
const esl_symbolic = require("esl_symbolic");

var dict = new buckets.MultiDictionary()

var s1 = esl_symbolic.string("s1");
var s2 = esl_symbolic.string("s2");
var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");

dict.set(s1, x1);
dict.set(s2, x2);

var res1 = dict.set(s1, undefined);
esl_symbolic.assert(!res1);

var s3 = esl_symbolic.string("s3");
esl_symbolic.assume(!(s1 == s3));
esl_symbolic.assume(!(s2 == s3));
var res2 = dict.get(s3).length;
esl_symbolic.assert(res2 == 0);

var res = dict.get(s1).length;

esl_symbolic.assert(((s1 == s2) && (!(x1 == x2)) && (res == 2)) || ((s1 == s2) && (x1 == x2) && (res == 1)) || ((!(s1 == s2)) && (res == 1)));
