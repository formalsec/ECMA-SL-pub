// -------------------------------- tests -------------------------------------
const buckets = require("../../src/set");
const esl_symbolic = require("esl_symbolic");

var set1 = new buckets.Set();
var set2 = new buckets.Set();

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");

esl_symbolic.assume(!(x1 == x3));

set1.add(x1);
set1.add(x2);

set2.add(x3);
set2.add(x2);

var res1 = set1.equals(set2);
esl_symbolic.assert(!res1);

set2.add(x1);
var res2 = set1.equals(set2);
esl_symbolic.assert(((x2 == x3) && res2) || ((!(x2 == x3)) && (!res2)));

var ar = [];
set2.forEach(function (x) {
  ar.push(x);
});

var res3 = set2.equals(ar);
esl_symbolic.assert(!res3);
