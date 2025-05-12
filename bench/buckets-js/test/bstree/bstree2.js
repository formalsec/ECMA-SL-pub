// ---------------------------- our tests now ---------------------------------
const buckets = require("./../../src/bstree");
const esl_symbolic = require("esl_symbolic");

var bst = new buckets.BSTree();

var x1 = esl_symbolic.number( "x1");
var x2 = esl_symbolic.number( "x2");
var x3 = esl_symbolic.number( "x3");
var x4 = esl_symbolic.number( "x4");

esl_symbolic.assume(!(x1 == x2));
esl_symbolic.assume(!(x1 == x3));
esl_symbolic.assume(!(x1 == x4));
esl_symbolic.assume(!(x2 == x3));
esl_symbolic.assume(!(x2 == x4));
esl_symbolic.assume(!(x3 == x4));

bst.add(x1);
bst.add(x2);
bst.add(x3);
bst.add(x4);

bst.remove(x2);
var res1 = bst.contains(x2);
esl_symbolic.assert(!res1);
var res2 = bst.contains(x3);
esl_symbolic.assert(res2);
var res0 = bst.contains(undefined);
esl_symbolic.assert(!res0);

bst.remove(x1);
bst.remove(x3);
bst.remove(x4);
var res3 = bst.contains(x1);
esl_symbolic.assert(!res3);
var res4 = bst.isEmpty();
esl_symbolic.assert(res4);
