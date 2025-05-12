var a, b, c, d;
try {
  try {
    a = "inside try try";
    throw "Error";
  } catch (ex) {
    b = "inside try catch";
    throw ex;
  } finally {
    c = "inside try finally";
  }
} catch (ex) {
  d = "inside catch";
}

var e;
try {
  try {
    throw "Throw in try try";
  } catch (ex) {
    throw "Throw in try catch";
  } finally {
    throw "Throw in finally";
  }
  throw "Throw in try";
} catch (ex) {
  e = ex;
}
