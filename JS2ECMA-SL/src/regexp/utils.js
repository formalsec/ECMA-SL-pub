module.exports = addParenIndexAndParenCount;

/**
 * Traversers the AST of the RegExp, transforms the regular expression
 * in place, applying the callback to each inner node, and returns 
 * a traversial state.
 * 
 * @param {Array<*>} re 
 * @param {*} st 
 * @param {Function} callback 
 * @returns A traversial state of the RegExp.
 */
function map(re, st, callbackPre, callbackPost) {

	let st1 = callbackPre(re, st);
	let st2;
	switch(re.type) {
		case 'Alternative':
		case 'CharacterClass':
			st2 = re.expressions.reduce( (acc, re2) =>  map(re2, acc, callbackPre, callbackPost), st1 );
			break;

		case 'Group':
		case 'Repetition':
			if (re.expression === null) st2 = st1;
			else st2 = map(re.expression, st1, callbackPre, callbackPost);
			break;

		case 'Disjunction':
			let stDis = st1;
			if (re.left !== null) {
				stDis = map(re.left, st1, callbackPre, callbackPost);
			}
			st2 = stDis;
			if (re.right !== null) { // Can be null. E.g.: /()|/
				st2 = map(re.right, stDis, callbackPre, callbackPost);
			}
			break;

		case 'RegExp':
			if (re.body === null) st2 = st1;
			else st2 = map(re.body, st1, callbackPre, callbackPost);
			break;

		default:
			st2 = st1;
			break;
	}

	return callbackPost(re, st2);
}

/**
 * Add parenIndex and parenCount to each node of a RegExp.
 * 
 * @param re The RegExp.
 */
function addParenIndexAndParenCount(re) {

	function callbackPre(re, st) {
		switch(re.type) {
			case 'Group':
				if (re.capturing) {
					return {
						parenIndex: st.parenIndex + 1,
						parenCount: st.parenCount + 1,
					};
				} else {
					return st;
				}

			default:
				return st;
		}
	}

	function callbackPost(re, st) {
		switch(re.type) {
			case 'Group':
				if (re.capturing) {
					return {
						parenIndex: st.parenIndex,
						parenCount: st.parenCount - 1,
					};
				} else {
					return st;
				}

			case 'Repetition':
				re.parenIndex = st.parenIndex;
				re.parenCount = st.parenCount;
				return st;
				
			default:
				return st;
		}
	}
	
	return map(re, { parenIndex: 0, parenCount: 0 }, callbackPre, callbackPost);
}
