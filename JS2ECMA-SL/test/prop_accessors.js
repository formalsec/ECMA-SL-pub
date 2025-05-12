obj = {
  p1: 1,
  p2: "2",
  p3: null,
};

a1 = obj.p1;
a2 = obj.p2;
a3 = obj.p3;

o1 = {
  q: 2
};

obj.__proto__ = o1;

a4 = obj.q;
