// ------------------------------- our tests ----------------------------------
const buckets = require("../../src/queue");
const esl_symbolic = require("esl_symbolic");


var queue = new buckets.Queue();

var x1 = esl_symbolic.number( "x1"); // 1
var x2 = esl_symbolic.number( "x2"); // 2
var x3 = esl_symbolic.number( "x3"); // 3
esl_symbolic.assume((x1 < x2) && (x2 < x3));

function createQueue() {
  queue.enqueue(x1);
  queue.enqueue(x2);
  queue.enqueue(x3);
}

var queue2 = new buckets.Queue();

var ar = [];

createQueue();

queue.forEach(function (x) {
  ar.push(x);
  queue2.enqueue(x);
});

var res1 = queue.equals(queue2);
esl_symbolic.assert(res1);

queue2.enqueue(x1);
var res2 = queue.equals(queue2);
esl_symbolic.assert(!res2);

var res3 = queue.equals(ar);
esl_symbolic.assert(!res3);
