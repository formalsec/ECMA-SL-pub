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

bst.clear();
var res = bst.isEmpty();
esl_symbolic.assert(res);
