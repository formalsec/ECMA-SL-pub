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
var res1 = stack.size();
esl_symbolic.assert(res1 == 0);
stack.push(n1);
stack.push(n2);
stack.push(n3);
var res2 = stack.size();
esl_symbolic.assert(res2 == 3);
var res3 = stack.isEmpty();
esl_symbolic.assert(!res3);
stack.clear();
var res4 = stack.isEmpty();
esl_symbolic.assert(res4);
