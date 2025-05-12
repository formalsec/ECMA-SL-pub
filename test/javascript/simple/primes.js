/**
 * * javascript/simple/primes.js
 * 
 * Function that checks if a number is prime. 
*/

function isPrime(value) {
	let primes = [];
	for (let i = 2; i <= value; i++) {
		primes[i] = true;
	}
	let limit = value;
	for (let i = 2; i <= limit; i++) {
		if (primes[i]) {
			for (let j = i * i; j <= value; j += i) {
				primes[j] = false;
			}
		}
	}
	return primes[value];
}

let isPrime10 = isPrime(10);
console.log("isPrime(10): " + isPrime10);
AssertEquals(isPrime10, false);

let isPrime23 = isPrime(23);
console.log("isPrime(23): " + isPrime23);
AssertEquals(isPrime23, true);
