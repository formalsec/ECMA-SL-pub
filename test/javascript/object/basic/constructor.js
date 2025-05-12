/**
 * * javascript/object/constructor.js
 * 
 * Creates an object with the 'new' keyword and uses the object prototype
 * to initialize its field.
 * @return true
*/

let obj = new Object();
Object.prototype.foo = 10;

AssertEquals(obj.foo, 10);
