/**
 * * javascript/object/methods/own_descriptor.js
 * 
 * Object.getOwnPropertyDescriptor - Gets the own property descriptor of 
 * the specified object. An own property descriptor is one that is defined 
 * directly on the object and is not inherited from the object's prototype.
 * @return true
*/

let proto = { foo: 10, bar: "abc" };
let obj = Object.create(proto);
obj.foo = 20;

let foo = Object.getOwnPropertyDescriptor(obj, "foo");
let bar = Object.getOwnPropertyDescriptor(obj, "bar");

AssertEquals(foo.value, 20);
AssertEquals(bar, undefined);
