o1 = {
  a: 1,
  b: 2
};
o2 = {
  c: 3,
  d: 4,
  e: 5
};
o3 = {
  a: 2,
  c: 4
};

o2.__proto__ = o1;
o3.__proto__ = o2;

a1 = 0;
for (var x in o2) {
  a1 = a1 + o2[x];
}

a2 = 0;
for(y in o3) {
  a2 = a2 + o3[y];
}
