// -------------------------------- tests -------------------------------------
const buckets = require("../../src/set");
const esl_symbolic = require("esl_symbolic");

var set1 = new buckets.Set();
var set2 = new buckets.Set();

var x1 = esl_symbolic.string("s1");
var x2 = esl_symbolic.string("s2");
var x3 = esl_symbolic.string("s3");
var x4 = esl_symbolic.string("s4");

set1.add(x1);
set1.add(x2);

set2.add(x3);
set2.add(x4);

var res1 = set2.isSubsetOf(set1);

set1.union(set2);

var res2 = set1.contains(x3);
var res3 = set2.isSubsetOf(set1);

esl_symbolic.assert((((((x3 == x1) || (x3 == x2)) && ((x4 == x1) || (x4 == x2))) && res1) || ((((!(x3 == x1)) && (!(x3 == x2))) || ((!(x4 == x1)) && (!(x4 == x2)))) && (!res1))) && (res2 && res3));
