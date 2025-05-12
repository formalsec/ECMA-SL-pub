/**
 * * javascript/object/basic/prop_keys.js
 * 
 * Creates an object with different datatypes for its keys.
 * @return true
*/

let obj = {
	"a": 1,
	"b": 2,
	10: 3,
	20: 4,
	[Symbol(1)]: 5,
}

let keys = Object.keys(obj);
keys.sort();

AssertArray(keys, ["10", "20", "a", "b"]);
