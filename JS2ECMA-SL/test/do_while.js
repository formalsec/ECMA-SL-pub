do_while_label1: do {
  a1 = 4;
  a2 = 0;
  inner_label: do {
    a1 = a1 - 1;
    if (a1 / 2 === 1) continue inner_label;
    a2 = a2 + 1;
  } while (a1 > 0);

  break do_while_label1;
} while (true);
