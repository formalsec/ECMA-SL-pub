// ------------------------------- our tests ----------------------------------
const buckets = require("../../src/priorityqueue");
const esl_symbolic = require("esl_symbolic");

var pqueue = new buckets.PriorityQueue();

var x1 = esl_symbolic.number("x1");
var x2 = esl_symbolic.number("x2");
var x3 = esl_symbolic.number("x3");

pqueue.enqueue(x1);
pqueue.enqueue(x2);
pqueue.enqueue(x3);

var ar = pqueue.toArray();
var l1 = pqueue.size();
var l2 = ar.length;
esl_symbolic.assert(l1 == l2);
var i;
for (i = 0; i < l1; i++) {
  var ari = ar[i];
  var resi = pqueue.contains(ari);
  esl_symbolic.assert(resi);
}
