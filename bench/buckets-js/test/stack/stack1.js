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
var res1 = stack.pop();
esl_symbolic.assert(res1 == undefined);
stack.push(n1);
stack.push(n2);
stack.add(n3);
var res6 = stack.peek();
var res2 = stack.pop();
var res3 = stack.pop();
var res4 = stack.pop();
var res5 = stack.pop();
esl_symbolic.assert(res2 == n3);
esl_symbolic.assert(res6 == n3);
esl_symbolic.assert(res3 == n2);
esl_symbolic.assert(res4 == n1);
esl_symbolic.assert(res5 == undefined);
