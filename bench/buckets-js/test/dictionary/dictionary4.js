// --------------------------------- tests -----------------------------------
const buckets = require("./../../src/dictionary");
const esl_symbolic = require("esl_symbolic");

var dict = new buckets.Dictionary();

var x1 = esl_symbolic.number("x1"); //1
var x2 = esl_symbolic.number("x2"); //2
var s1 = esl_symbolic.string("s1"); // "2"
var s2 = esl_symbolic.string("s2"); // "foo"

esl_symbolic.assume(!(s1 == s2));

dict.set(s1, x1);
dict.set(s2, x2);


var res1 = "";
var res2 = 0;
var s3 = esl_symbolic.string("s3");

dict.forEach(function(k, v) {
  if (k === s3) {
    return false;
  }
  res1 = res1 + k;
  res2 = res2 + v;
});

var c1 = s1 + s2;
var c2 = s2 + s1;
esl_symbolic.assert(((!((s3 == s1) || (s3 == s2))) && ((res1 == c1) || (res1 == c2))) || (((s3 == s1) || (s3 == s2)) && ((res1 == s1) || (res1 == s2) || (res1 == ""))));

var y = x1 + x2;
esl_symbolic.assert(((!((s3 == s1) || (s3 == s2))) && (res2 == y)) || (((s3 == s1) || (s3 == s2)) && ((res2 == x1) || (res2 == x2) || (res2 == 0))));
