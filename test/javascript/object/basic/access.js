/**
 * * javascript/object/basic/access.js
 * 
 * Creates an empty object, initializes a fields with multiple data types and
 * retrieves their value.
 * @return true
*/

let obj = {};
obj.foo = 10;
obj.bar = "abc";
obj.baz = { qux: true };

AssertEquals(obj.foo, 10);
AssertEquals(obj.bar, "abc");
AssertEquals(obj.baz.qux, true);
