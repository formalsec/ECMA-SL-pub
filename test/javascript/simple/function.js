/**
 * * javascript/simple/function.js
 * 
 * Function that returns a context-dependent inner function.
*/

function foo(x) {
	let value = x;
	return () => {
		return value + 10;
	};
}

console.log("Function 'foo' called");
let func = foo(10);
console.log("Function 'func' called");
let ret = func();
console.log("Ret: " + ret);

AssertEquals(ret, 20);
