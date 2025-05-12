// ------------------------------ our tests now ------------------------------
const buckets = require("./../../src/bag");
const esl_symbolic = require("esl_symbolic");

// init
var bag = new buckets.Bag();

// size
var n1 = esl_symbolic.number("n1");
var n2 = esl_symbolic.number("n2");
var n3 = esl_symbolic.number("n3");

bag.add(n1);
bag.add(n2);

var bag2 = new buckets.Bag();

var res1 = bag.equals(bag2);
esl_symbolic.assert(!res1);

var res3 = bag.equals([n1, n2, n3]);
esl_symbolic.assert(!res3);

bag.forEach(function(x) {
  if (x == n3) {
    return false;
  }
  bag2.add(x);
});

var res2 = bag.equals(bag2);
esl_symbolic.assert(((!(n3 == n1)) && (!(n3 == n2)) && res2) || (((n3 == n1) || (n3 == n2)) && (!res2)));
