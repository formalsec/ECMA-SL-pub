// --------------------------------- tests -----------------------------------
const buckets = require("./../../src/dictionary");
const esl_symbolic = require("esl_symbolic");

var dict = new buckets.Dictionary();

var x1 = esl_symbolic.number( "x1"); //1
var x2 = esl_symbolic.number( "x2"); //2
var s1 = esl_symbolic.string("s1"); // "2"
var s2 = esl_symbolic.string("s2"); // "foo"

esl_symbolic.assume(!(s1 == s2));

dict.set(s1, x1);
dict.set(s2, x2);

var dict2;
var res2 = dict.equals(dict2);
esl_symbolic.assert(!res2);
dict2 = new buckets.Dictionary();

var res1 = dict.equals(dict2);
esl_symbolic.assert(!res1);

var keys = dict.keys();
var vals = dict.values();

var i = 0;
for (i = 0; i < 2; i++) {
  dict2.set(keys[i], vals[i]);
}

var res3 = dict2.equals(dict);
esl_symbolic.assert(res3);
