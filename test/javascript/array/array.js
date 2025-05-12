/**
 * * javascript/simple/array.js
 * 
 * Loop statement that iterates through an array and sums all the numbers stored within.
 * @return true
*/

let arr = [1, 2, 3];
arr[3] = 4;

let ret = 0;
for (let i = 0; i < arr.length; i++) {
	ret += arr[i];
}

AssertEquals(ret, 10);
