function f(x, y) {
  var z = x + y;

  function g(x) {
    var g1;
    return multiplication(5)(x) + y;
  }

  return g(z);
}

var multiplication = function c(multi) {
  var m1;
  return function d(x) {
    var mm1;
    return x * multi;
  };
};

var ret = f(1, 2);
