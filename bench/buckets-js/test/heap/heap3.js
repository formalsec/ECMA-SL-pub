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

heap.add(x1);
heap.add(x2);
heap.add(x3);
//heap.add(x4);

heap.removeRoot();
var res1 = heap.contains(x1);
esl_symbolic.assert(((x1 < x2) && (x1 < x3) && (!res1)) || ((!((x1 < x2) && (x1 < x3))) && res1));

var res2 = heap.size();
esl_symbolic.assert(res2 == 2);

heap.clear();
var res3 = heap.isEmpty();
esl_symbolic.assert(res3);
var res4 = heap.removeRoot();
esl_symbolic.assert(res4 == undefined);
