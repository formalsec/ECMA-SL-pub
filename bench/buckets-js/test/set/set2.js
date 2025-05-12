// -------------------------------- tests -------------------------------------
const buckets = require("../../src/set");
const esl_symbolic = require("esl_symbolic");

var set1 = new buckets.Set();
var set2 = new buckets.Set();

var x1 = esl_symbolic.number("x1");
var x2 = esl_symbolic.number("x2");
var x3 = esl_symbolic.number("x3");

esl_symbolic.assume(!(x1 == x2));

set1.add(x1);
set1.add(x2);

set2.add(x2);
set2.add(x3);

set1.intersection(set2);

var res1 = set1.contains(x2);
esl_symbolic.assert(res1);

var res2 = set1.contains(x1);
esl_symbolic.assert((((x1 == x3)) && res2) || ((!(x1 == x3)) && (!res2)));
