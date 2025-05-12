// -------------------------------- tests -------------------------------------
const buckets = require("../../src/stack");
const esl_symbolic = require("esl_symbolic");

var stack = new buckets.Stack();

var n1 = esl_symbolic.number( "x1");
var n2 = esl_symbolic.number( "x2");
var n3 = esl_symbolic.number( "x3");
esl_symbolic.assume(!(n1 == n2));
esl_symbolic.assume(!(n1 == n3));
esl_symbolic.assume(!(n2 == n3));


// test 1
//it('pop returns and removes the top element or undefined', function () {
stack.push(n1);
stack.push(n2);
stack.push(n3);
stack.pop();
var res1 = stack.contains(n3);
esl_symbolic.assert(!res1);
stack.pop();
var res2 = stack.contains(n1);
esl_symbolic.assert(res2);

