// --------------------------------- tests -----------------------------------
const buckets = require("./../../src/dictionary");
const esl_symbolic = require("esl_symbolic");

var dict = new buckets.Dictionary();

var x1 = esl_symbolic.number("x1"); //1
var x2 = esl_symbolic.number("x2"); //2
var s1 = esl_symbolic.string("s2"); // "2"
var s2 = esl_symbolic.string("s1"); // "foo"

dict.set(s1, x1);
var res1 = dict.set(s2, x2);
esl_symbolic.assert(((s1 == s2) && (res1 == x1)) || ((!(s1 == s2)) && (res1 == undefined)));


var res2 = dict.set(s1, undefined);
esl_symbolic.assert(res2 == undefined);

var s3 = esl_symbolic.string("s3");
esl_symbolic.assume(!(s3 == s1));
esl_symbolic.assume(!(s3 == s2));

var res3 = dict.get(s3);
esl_symbolic.assert(res3 == undefined);

var res = dict.get(s1);

esl_symbolic.assert(
    ( (s1 == s2) && (x1 == x2) && (res == x1))
      || ((s1 == s2) && (!(x1 == x2)) && (res == x2))
      || ((!(s1 == s2)) && (res == x1)));

