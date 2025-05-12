a1 = 3;
a2 = 0;
while (a1 > a2) {
  a2 = a2 + 1;
}

b1 = 3;
while_label1: while (b1 > 0) {
  b1 = b1 - 1;
  b2 = b1 + 2;
  while (b2 > 0) {
    if (b2 / 2 === 2) break while_label1;
    b2 = b2 - 1;
  }
}
