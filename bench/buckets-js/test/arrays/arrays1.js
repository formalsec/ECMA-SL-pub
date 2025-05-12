// ------------------------------ our test now -------------------------------
const buckets = require("./../../src/arrays");
const esl_symbolic = require("esl_symbolic");

var n1 = esl_symbolic.number("n1"); // 1
var n2 = esl_symbolic.number("n2"); // 8
var n3 = esl_symbolic.number("n3"); // 10
var n4 = esl_symbolic.number("n4"); // 42

esl_symbolic.assume(!(n1 == n2));
esl_symbolic.assume(!(n1 == n3));
esl_symbolic.assume(!(n1 == n4));

esl_symbolic.assume(!(n2 == n3));
esl_symbolic.assume(!(n2 == n4));
//esl_symbolic.assume(n2 > 1 && n2 < 10);

esl_symbolic.assume(!(n3 == n4));


var numberArray = [n1, n2, n2, n2, n3, n3];

var reset = function() {
  numberArray = [n1, n2, n2, n2, n3, n3];
}

// initial setup
reset();

// indexOf
var res2 = buckets.arrays.indexOf(numberArray, n2);
esl_symbolic.assert(res2 == 1);
var res3 = buckets.arrays.indexOf(numberArray, n4);
esl_symbolic.assert(res3 == -1);
