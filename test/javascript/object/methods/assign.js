/**
 * * javascript/object/methods/assign.js
 * 
 * Object.assign - Copy the values of all of the enumerable own properties 
 * from one or more source objects to a target object.
 * @return true
*/

var source = { foo: 1, bar: 4 };
var target = { bar: 2, baz: 5 };
var newTarget = Object.assign(target, source);

AssertObject(source, { foo: 1, bar: 4 });
AssertObject(target, { foo: 1, bar: 4, baz: 5 });
AssertObject(newTarget, { foo: 1, bar: 4, baz: 5 });
