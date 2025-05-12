/**
 * * javascript/simple/destructuring.js
 * 
 * Destructuring multiple complex JS values into simple variables.  
*/

// Empty object pattern
var a = ({} = {});
AssertEquals(a, {});
console.log("Successful destructuring of 'a'");

// Object pattern with two fields, but only one initialization (no defaults)
var b1, b2;
var b = ({ b1, b2 } = { b1: 10 });
AssertEquals(b1, 10);
AssertEquals(b2, undefined);
AssertEquals(b, { b1: 10 });
console.log("Successful destructuring of 'b'");

// Object pattern with two fields, but only one initialization (defaults)
var c1, c2;
var c = ({ c1, c2 = 20 } = { c1: 10 });
AssertEquals(c1, 10);
AssertEquals(c2, 20);
AssertEquals(c, { c1: 10 });
console.log("Successful destructuring of 'c'");

// Object pattern with an inner pattern initialized
var d1, d11;
var d = ({ d1: { d11 } } = { d1: { d11: 10 } });
AssertEquals(d11, 10);
AssertEquals(d1, undefined);
AssertEquals(d, { d1: { d11: 10 } });
console.log("Successful destructuring of 'd'");

// Object pattern with an inner pattern non initialized (defaults)
var e1, e11;
var e = ({ e1: { e11 } = { e11: 10 } } = {});
AssertEquals(e11, 10);
AssertEquals(e1, undefined);
AssertEquals(e, {});
console.log("Successful destructuring of 'e'");

// Object pattern with a rest property
var f1, ftl;
var f = ({ f1, ...ftl } = { f1: 10, f2: 20, f3: 30 });
AssertEquals(f1, 10);
AssertEquals(ftl, { f2: 20, f3: 30 });
AssertEquals(f, { f1: 10, f2: 20, f3: 30 });
console.log("Successful destructuring of 'f'");

// Empty array pattern
var g = ([] = []);
AssertEquals(g, []);
console.log("Successful destructuring of 'g'");

// Array pattern with two elements, but only one initialization (no defaults)
var h1, h2;
var h = ([h1, h2] = [10]);
AssertEquals(h1, 10);
AssertEquals(h2, undefined);
AssertEquals(h, [10]);
console.log("Successful destructuring of 'h'");

// Array pattern with two elements, but only one initialization (defaults)
var i1, i2;
var i = ([i1, i2 = 20] = [10]);
AssertEquals(i1, 10);
AssertEquals(i2, 20);
AssertEquals(i, [10]);
console.log("Successful destructuring of 'i'");

// Array pattern with two elements and an elision
var j1, j3;
var j = ([j1, , j3] = [10, 20, 30]);
AssertEquals(j1, 10);
AssertEquals(j3, 30);
AssertEquals(j, [10, 20, 30]);
console.log("Successful destructuring of 'j'");

// Array pattern with a rest property
var k1, ktl;
var k = ([k1, ...ktl] = [10, 20, 30]);
AssertEquals(k1, 10);
AssertEquals(ktl, [20, 30]);
AssertEquals(k, [10, 20, 30]);
console.log("Successful destructuring of 'k'");
