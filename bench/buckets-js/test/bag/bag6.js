// ------------------------------ our tests now ------------------------------
const buckets = require("./../../src/bag");
const esl_symbolic = require("esl_symbolic");

// init
var bag = new buckets.Bag();

// size
var n1 = esl_symbolic.number("n1");
var n2 = esl_symbolic.number("n2");
var n3 = esl_symbolic.number("n3");
var n4 = esl_symbolic.number("n4");

var res1 = bag.isEmpty();
esl_symbolic.assert(res1);
var res2 = bag.size();
esl_symbolic.assert(res2 == 0);

bag.add(n1);
bag.add(n1, n2);
var res3 = bag.isEmpty();
esl_symbolic.assert(!res3);
bag.remove(n1);
bag.remove(n3, n4);
bag.clear();
var res4 = bag.isEmpty();
esl_symbolic.assert(res4);
