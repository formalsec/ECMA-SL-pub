/**
 * * javascript/simple/object.js
 * 
 * Object with three recursive mathematical operations stored properties. 
*/

function Math(base) {
	this.base = base;

	this.sum = function sum(x) {
		if (x < 1) {
			return 0;
		} else {
			return this.sum(x - 1) + x;
		}
	};

	this.fib = function fib(x) {
		if (x < 3) {
			return 1;
		} else {
			return this.fib(x - 1) + this.fib(x - 2);
		}
	};

	this.fact = function fact(x) {
		if (x < 1) {
			return 1;
		} else {
			return this.fact(x - 1) * x;
		}
	};

	this.total = function total() {
		let total = 0;
		total += math.sum(this.base);
		console.log("Method 'sum' called");
		console.log("Total: " + total);

		total += math.fib(this.base);
		console.log("Method 'fib' called");
		console.log("Total: " + total);

		total += math.fact(this.base);
		console.log("Method 'fact' called");
		console.log("Total: " + total);
		return total;
	};

}

let math = new Math(4);
let total = math.total();

AssertEquals(total, 37);
