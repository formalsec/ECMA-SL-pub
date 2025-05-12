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


// TEST 1

//it('size gives the right value', function () {
var queue = new buckets.Queue();
createQueue();
queue.dequeue();
queue.dequeue();
var res1 = queue.peek();
queue.dequeue();
queue.dequeue();
var res2 = queue.peek();
esl_symbolic.assert(res1 == x3);
esl_symbolic.assert(res2 == undefined);
