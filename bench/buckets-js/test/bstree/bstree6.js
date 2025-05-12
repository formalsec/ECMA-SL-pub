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

// test2: something fails

var x4 = esl_symbolic.number( "x4");
var ar2 = [];
bst.preorderTraversal(function(x) {
  if (x === x4) {
    return false;
  }
  ar2.push(x);
});
var l2 = ar2.length;
esl_symbolic.assert(((!(x4 == x1)) && (!(x4 == x2)) && (!(x4 == x3)) && (l2 == 3)) || (((x4 == x1) || (x4 == x2) || (x4 == x3)) && (l2 < 3)));
