/**
 * * javascript/simple/catch.js
 * 
 * Function that returns a context-dependent with an exception.
*/

function foo(x) {
	let value = x;
	return () => {
		throw value + 10;
	};
}

let func = foo(10);
let ret = -1;

try {
	console.log("Function 'foo' called");
	ret = func();
} catch (e) {
	console.log("Exception caught: " + e);
	ret = e + 10;
}

console.log("Ret: " + ret);
AssertEquals(ret, 30);
