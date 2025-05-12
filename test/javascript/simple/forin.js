/**
 * * javascript/simple/forin.js
 * 
 * For-in loop statement that sums all the properties of an object. 
*/

function sumProps(obj) {
	let total = 0;
	for (let fld in obj) {
		total += obj[fld];
		console.log("Added field '" + fld +  "': " + total);
	}
	return total;
}

let obj = { foo: 1, bar: 2, baz: 3, qux: 4 };
let total = sumProps(obj);

AssertEquals(total, 10);
