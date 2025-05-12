// ------------------------------ our tests now ------------------------------
const buckets = require("./../../src/bag");
const esl_symbolic = require("esl_symbolic");

// init
var bag = new buckets.Bag();

// size
var n1 = esl_symbolic.number( "n1");
var n2 = esl_symbolic.number( "n2");
var n3 = esl_symbolic.number( "n3");
var n4 = esl_symbolic.number( "n4");

bag.add(n1);
bag.add(n1, n2);
bag.remove(n1);
var res1 = bag.remove(n3, n4);
var res2 = bag.count(n1);
esl_symbolic.assert(((n3 == n1) && (((n2 > 0) && (n4 > 0) && (n4 <= n2) && res1 && (res2 == (n2 - n4))) || ((n2 > 0) && (n4 > 0) && (n4 > n2) && res1 && (res2 == 0)) || ((n2 >= 0) && (n4 <= 0) && (!res1) && (res2 == n2)) || ((n2 <= 0) && (!res1) && (res2 == 0)))) || ((!(n3 == n1)) && (!res1) && (((n2 >= 0) && (res2 == n2)) || ((n2 < 0) && (res2 == 0)))));
