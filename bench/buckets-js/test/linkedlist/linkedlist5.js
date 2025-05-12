// ---------------------------------- tests ----------------------------------
const buckets = require("./../../src/linkedlist");
const esl_symbolic = require("esl_symbolic");

var list1 = new buckets.LinkedList();
var list2 = new buckets.LinkedList();

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");
var x4 = esl_symbolic.number( "x4");

list1.add(x1);
list1.add(x2);

list2.add(x3);
var res1 = list1.equals(list2);
esl_symbolic.assert(!res1);

list2.add(x4);
var res2 = list1.equals(list2);
esl_symbolic.assert(((x1 == x3) && (x2 == x4) && res2) || ((!((x1 == x3) && (x2 == x4))) && (!res2)));

var res3 = list2.equals([x3, x4]);
esl_symbolic.assert(!res3);
