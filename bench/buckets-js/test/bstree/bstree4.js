// ---------------------------- our tests now ---------------------------------
const buckets = require("./../../src/bstree");
const esl_symbolic = require("esl_symbolic");

var bst = new buckets.BSTree();

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");

esl_symbolic.assume(!(x1 == x2));
esl_symbolic.assume(!(x1 == x3));
esl_symbolic.assume(!(x2 == x3));

bst.add(x1);
bst.add(x2);
bst.add(x3);

// test 1: full traversal
var ar1 = [];
bst.inorderTraversal(function (x) {
  ar1.push(x);
});

var l = ar1.length;
esl_symbolic.assert(l == 3);

var y1 = ar1[0];
var y2 = ar1[1];
var y3 = ar1[2];
esl_symbolic.assert((y1 < y2) && (y2 < y3));
