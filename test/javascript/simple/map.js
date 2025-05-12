/**
 * * javascript/simple/map.js
 * 
 * Simple implementation of a concrete map with functions to store and retrieve bindings. 
*/

function Map() {
	this._contents = {};
}

function isValidKey(key) {
	return typeof key === 'string' && key !== '';
}


Map.prototype.get = function get(key) {
	if (isValidKey(key)) {
		if (this._contents.hasOwnProperty(key)) {
			return this._contents[key];
		} else {
			return null;
		}
	} else {
		throw new Error("invalid key")
	}
}


Map.prototype.put = function put(key, value) {
	if (isValidKey(key)) {
		this._contents[key] = value;
	} else {
		throw new Error("invalid key")
	}
}

let map = new Map();
map.put("foo", 10);
map.put("bar", 20);

let foo = map.get("foo");
console.log("Key 'foo': " + foo);
AssertEquals(foo, 10);

let bar = map.get("bar");
console.log("Key 'bar': " + bar);
AssertEquals(bar, 20);
