// -------------------------------- tests -------------------------------------
const buckets = require("../../src/set");
const esl_symbolic = require("esl_symbolic");

var set = new buckets.Set();

var x1 = esl_symbolic.number("x1");
var x2 = esl_symbolic.number("x2");

set.add(x1);
var res = set.add(x2);

var x3 = esl_symbolic.number("x3");
var res2 = set.contains(x3);

