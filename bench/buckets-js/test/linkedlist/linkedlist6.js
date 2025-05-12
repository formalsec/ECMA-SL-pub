// ---------------------------------- tests ----------------------------------
const buckets = require("./../../src/linkedlist");
const esl_symbolic = require("esl_symbolic");

var list = new buckets.LinkedList()

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");
var x4 = esl_symbolic.number( "x4");

// esl_symbolic.assume(!(x1 == x2));
// esl_symbolic.assume(!(x1 == x3));
// esl_symbolic.assume(!(x2 == x3));

list.add(x1);
list.add(x2);
list.add(x3);

var list2 = new buckets.LinkedList();
list.forEach(function (x) {
  list2.add(x);
});

list.reverse();
var res = list.equals(list2);
esl_symbolic.assert(((x1 == x3) && res) || ((!(x1 == x3)) && (!res)));
