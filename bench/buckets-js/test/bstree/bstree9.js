// ---------------------------- our tests now ---------------------------------
const buckets = require("./../../src/bstree");
const esl_symbolic = require("esl_symbolic");

var bst = new buckets.BSTree();

var x1 = esl_symbolic.number("x1");
var x2 = esl_symbolic.number("x2");
var x3 = esl_symbolic.number("x3");

esl_symbolic.assume(!(x1 == x2));
esl_symbolic.assume(!(x1 == x3));
esl_symbolic.assume(!(x2 == x3));

var r1 = bst.minimum();
esl_symbolic.assert(r1 == undefined);
var r2 = bst.maximum();
esl_symbolic.assert(r2 == undefined);

bst.add(x1);
bst.add(x2);
bst.add(x3);

var res1 = bst.minimum();
esl_symbolic.assert(((x1 < x2) && (x2 < x3) && (res1 == x1)) || ((x1 < x3) && (x3 < x2) && (res1 == x1)) || ((x2 < x1) && (x1 < x3) && (res1 == x2)) || ((x2 < x3) && (x3 < x1) && (res1 == x2)) || ((x3 < x1) && (x1 < x2) && (res1 == x3)) || ((x3 < x2) && (x2 < x1) && (res1 == x3)));

var res2 = bst.maximum();
esl_symbolic.assert(((x1 < x2) && (x2 < x3) && (res2 == x3)) || ((x1 < x3) && (x3 < x2) && (res2 == x2)) || ((x2 < x1) && (x1 < x3) && (res2 == x3)) || ((x2 < x3) && (x3 < x1) && (res2 == x1)) || ((x3 < x1) && (x1 < x2) && (res2 == x2)) || ((x3 < x2) && (x2 < x1) && (res2 == x1)));
