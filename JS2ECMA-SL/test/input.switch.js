a = "aa";
switch (a) {
  case "aa":
    a1 = "Correct!";
    break;
  default:
    a1 = "Incorrect!";
  case "ab":
    a1 = "Almost!";
    break;
}

switch (a) {
  case "aa":
    a2 = "Incorrect!";
  default:
    a2 = "Almost!";
  case "ab":
    a2 = "Correct!";
    break;
}

ab = "ab";
switch (ab) {
  case "aa":
    ab1 = "Incorrect!";
  default:
    ab1 = "Incorrect!";
  case "ab":
    ab1 = "Correct!";
    break;
}

ac = "ac";
switch (ac) {
  case "aa":
    ac1 = "Incorrect!";
  default:
    ac1 = "Incorrect!";
  case "ac":
    ac1 = "Almost!";
  case "ab":
    ac1 = "Correct!";
}

b = "bb";
switch (b) {
  case "bb":
    b1 = "Incorrect!";
  case "bbb":
    b1 = "Correct!";
}

c = "cc";
c1 = "Incorrect!";
switch (c) {
  default:
    c1 = "Correct!";
}

d = "dd";
switch (d) {
}
d1 = "Correct!";
