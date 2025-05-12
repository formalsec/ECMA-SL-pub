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

esl_symbolic.assume(!(n3 == n4));


var numberArray = [n1, n2, n2, n2, n3, n3];

var reset = function() {
  numberArray = [n1, n2, n2, n2, n3, n3];
}

// initial setup
reset();

// copy
var array2 = buckets.arrays.copy(numberArray);
var res = buckets.arrays.equals(array2, numberArray);
esl_symbolic.assert(res);
