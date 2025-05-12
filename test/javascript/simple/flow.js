/**
 * * javascript/simple/flow.js
 * 
 * Loop statement that produces the sums all even numbers from 1 to 100.
*/

let total = 0;
for (let i = 0; i < 100; i++) {
	if (i % 2 == 0) {
		total += i;
	}
}

console.log("Total: " + total);
AssertEquals(total, 2450);
