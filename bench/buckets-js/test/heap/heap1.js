// ------------------------------ our tests now ------------------------------
const buckets = require("./../../src/heap");
const esl_symbolic = require("esl_symbolic");

var heap = new buckets.Heap();

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");
var x4 = esl_symbolic.number( "x4");

esl_symbolic.assume(!(x1 == x2));
esl_symbolic.assume(!(x1 == x3));
esl_symbolic.assume(!(x1 == x4));

esl_symbolic.assume(!(x2 == x3));
esl_symbolic.assume(!(x2 == x4));

esl_symbolic.assume(!(x3 == x4));


var res2 = heap.add(undefined);
esl_symbolic.assert(res2 == undefined);
var res3 = heap.peek();
esl_symbolic.assert(res3 == undefined);

heap.add(x1);
heap.add(x2);
heap.add(x3);
heap.add(x4);

var res1 = heap.peek();
