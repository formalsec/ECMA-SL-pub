/**
 * * javascript/simple/json.js
 * 
 * Simple usage of the JSON parser over exponential numbers.
*/

let parsed = JSON.parse('1e3');
console.log("Parsing '1e3': " + parsed);
AssertEquals(parsed, 1000);

parsed = JSON.parse('1.0e3');
console.log("Parsing '1.0e3': " + parsed);
AssertEquals(parsed, 1000);

parsed = JSON.parse('1.0E3');
console.log("Parsing '1.0E3': " + parsed);
AssertEquals(parsed, 1000);

parsed = JSON.parse('1.0E+3');
console.log("Parsing '1.0E+3': " + parsed);
AssertEquals(parsed, 1000);

parsed = JSON.parse('1e-3');
console.log("Parsing '1e-3': " + parsed);
AssertEquals(parsed, 0.001);
