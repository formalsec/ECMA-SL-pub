a1 = {
  p1: "a",
  p2: "b",
  get p3() {
    return this.p;
  },
  set p3(v) {
    this.p = v;
  },
};
a2 = {
  p2: 1,
  p2: 2,
};
a3 = {
  1: a2,
  2: 2,
};

a1.p3 = "c";
a4 = {
  p: a1.p3,
};
