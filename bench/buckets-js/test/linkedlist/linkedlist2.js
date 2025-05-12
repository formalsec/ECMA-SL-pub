// ---------------------------------- tests ----------------------------------
const buckets = require("./../../src/linkedlist");
const esl_symbolic = require("esl_symbolic");

var list = new buckets.LinkedList()

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");
var x4 = esl_symbolic.number( "x4");

esl_symbolic.assume(!(x1 == x2));
esl_symbolic.assume(!(x1 == x3));
esl_symbolic.assume(!(x2 == x3));

var res0 = list.first();
var res1 = list.last();
esl_symbolic.assert(res0 == undefined);
esl_symbolic.assert(res1 == undefined);

list.add(x1)
list.add(x2)
list.add(x3, x4)

var res3 = list.first();
esl_symbolic.assert(((x4 == 0) && (res3 == x3)) || (res3 == x1));
var res4 = list.last();
esl_symbolic.assert(((x4 == 2) && (res4 == x3)) || (res4 == x2));
