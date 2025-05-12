/**
 * * javascript/object/methods/freeze.js
 * 
 * Object.freeze - Prevents the modification of existing property attributes 
 * and values, and prevents the addition of new properties.
 * @return true
*/

let obj = { foo: 10 };

Object.freeze(obj);
obj.foo = 20;

AssertEquals(obj.foo, 10);
