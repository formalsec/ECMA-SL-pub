/**
 * * javascript/object/methods/create.js
 * 
 * Object.create - Creates an object that has the specified prototype or that 
 * has null prototype.
 * @return true
*/

let proto = { foo: 10, bar: "abc" };
let obj = Object.create(proto);

AssertEquals(obj.foo, 10);
AssertEquals(obj.bar, "abc");
