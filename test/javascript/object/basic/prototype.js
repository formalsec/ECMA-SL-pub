/**
 * * javascript/object/basic/prototype.js
 * 
 * Simple object with a basic prototype inheritance.
 * @return true
*/

let proto = { foo: 10, bar: "abc" };
let obj = {};

Object.setPrototypeOf(obj, proto);

AssertEquals(obj.foo, 10);
AssertEquals(obj.bar, "abc");
