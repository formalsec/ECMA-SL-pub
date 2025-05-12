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

createQueue();

var ar1 = queue.toArray();
var ar2 = [];

queue.forEach(function (x) {
  ar2.push(x);
});

var l1 = queue.size(), l2 = ar1.length, l3 = ar2.length;
esl_symbolic.assert(l1 == l2);
esl_symbolic.assert(l2 == l3);

var i;
for (i = 0; i < ar1.length; i++) {
  var ar1i = ar1[i], ar2i = ar2[i];
  esl_symbolic.assert(ar1i == ar2i);
}
