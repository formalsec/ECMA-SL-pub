// ------------------------------- our tests ----------------------------------
const buckets = require("../../src/queue");
const esl_symbolic = require("esl_symbolic");

// modeling the tests from test/queue-test.js

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


// TEST 1

//it('size gives the right value', function () {
var queue = new buckets.Queue();
var size = queue.size();
esl_symbolic.assert(size == 0);
createQueue();
var size = queue.size();
esl_symbolic.assert(size == 3);
var x4 = esl_symbolic.number( "x4");
queue.add(x4); // synonym to enqueue
var size = queue.size();
esl_symbolic.assert(size == 4);
queue.dequeue();
var size = queue.size();
esl_symbolic.assert(size == 3);
queue.clear();
var size = queue.size();
esl_symbolic.assert(size == 0);
queue.clear();
