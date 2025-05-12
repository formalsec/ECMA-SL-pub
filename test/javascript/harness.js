/**
 * * javascript/simple/harness.js
 * 
 * Harness for the simple javascript tests.
*/

function JSAssertError(message) {
	this.message = message || "";
}

function AssertArray(arr, exp) {
	AssertEquals(arr.length, exp.length);
	arr.forEach((element, index) => AssertEquals(element, exp[index]));
}

function AssertObject(obj, exp) {
	AssertEquals(Object.keys(obj).length, Object.keys(exp).length);
	Object.keys(obj).forEach((fld, _) => AssertEquals(obj[fld], exp[fld]));
}

function AssertEquals(val, exp) {
	if (val !== exp) {
		if (typeof val == typeof exp && typeof exp == "object") {
			if (Array.isArray(exp)) { AssertArray(val, exp) }
			else { AssertObject(val, exp) }
		} else {
			throw new JSAssertError("Assertion failed!");
		}
	}
}
