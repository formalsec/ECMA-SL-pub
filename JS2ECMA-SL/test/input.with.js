var a, b;
var obj = {
  p1: 2,
  p2: "a",
};

with (obj) {
  a = p1 * 3;
  if (p2 === "a") {
    b = "correct";
  } else {
    b = "incorrect";
  }
}
