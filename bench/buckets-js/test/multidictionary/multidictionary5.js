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

var res1 = 0;
var sum1 = x1 + x2 + x3;
var sum2 = x1 + x2 + x2 + x3;
dict.forEach(function (k, vs) {
  var l = vs.length;
  var i;
  for (i = 0; i < l; i++) {
    res1 += vs[i];
  }
});

esl_symbolic.assert(((s1 == s2) && (res1 == sum1)) || ((!(s1 == s2)) && (res1 == sum2)));
