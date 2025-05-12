// ---------------------------------- tests ----------------------------------
const buckets = require("./../../src/linkedlist");
const esl_symbolic = require("esl_symbolic");

var list = new buckets.LinkedList()

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");
var x4 = esl_symbolic.number( "x4");

esl_symbolic.assume(!(x1 == x2));
esl_symbolic.assume(!(x1 == x3));
esl_symbolic.assume(!(x2 == x3));

list.add(x1)
list.add(x2)
list.add(x3);

var ar = list.toArray();

var l1 = list.size();
var l2 = ar.length;
esl_symbolic.assert(l1 == l2);

var i = 0
for (i = 0; i < l1; i++) {
  var li = list.elementAtIndex(i);
  var ari = ar[i];
  esl_symbolic.assert(li == ari);
}
