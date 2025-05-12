// ------------------------------- our tests ----------------------------------
const buckets = require("../../src/priorityqueue");
const esl_symbolic = require("esl_symbolic");

var pqueue = new buckets.PriorityQueue();

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");

pqueue.enqueue(x1);
pqueue.enqueue(x2);
pqueue.enqueue(x3);

pqueue.dequeue();

var pqueue2 = new buckets.PriorityQueue();

var res1 = pqueue.equals(pqueue2);
esl_symbolic.assert(!res1);

pqueue.forEach(function (x) {
  pqueue2.add(x);
});
var res2 = pqueue2.equals(pqueue);
esl_symbolic.assert(res2);

var ar = pqueue.toArray();
var res3 = pqueue.equals(ar);
esl_symbolic.assert(!res3);
