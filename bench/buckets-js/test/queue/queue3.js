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
}


// TEST 1

//it('size gives the right value', function () {
createQueue();
var res1 = queue.contains(x1);
var res2 = queue.contains(x3);
esl_symbolic.assert(res1);
esl_symbolic.assert(!res2);
