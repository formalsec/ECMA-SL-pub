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

var res = list.removeElementAtIndex(x4);
esl_symbolic.assert(((x4 == 0) && (res == x1)) || ((x4 == 1) && (res == x2)) || ((x4 == 2) && (res == x3)) || (res == undefined));
list.removeElementAtIndex(0);
list.removeElementAtIndex(0);
var res2 = list.isEmpty();

esl_symbolic.assert( ((((x4 == 0) || (x4 == 1)) || (x4 == 2)) && res2) || ((((!(x4 == 0)) && (!(x4 == 1))) && (!(x4 == 2))) && (!res2)) );
