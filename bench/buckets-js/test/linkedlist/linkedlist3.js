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

list.add(x1)
list.add(x2);
list.add(x3, x4)

var res1 = list.contains(x3);
esl_symbolic.assert ( (((x4 == 0) || (x4 == 1) || (x4 == 2)) && res1) || ((!(x4 == 0)) && (!(x4 == 1)) && (!(x4 == 2)) && (!res1)) );

var res2 = list.contains(undefined);
esl_symbolic.assert(!res2);
