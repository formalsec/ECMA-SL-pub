/**
 * * javascript/simple/queue.js
 * 
 * Simple implementation of a priority queue with functions to enqueue and
 * dequeue elements. 
*/

let PriorityQueue = function () {
	let nElements = 0;

	let Node = function (priority, value) {
		this.priority = priority;
		this.value = value;
		this.next = null;
		nElements++
	};

	Node.prototype.insert = function (node) {
		if (node === null) {
			return this;
		}

		if (this.priority > node.priority) {
			this.next = node;
			return this;
		}

		let temp = this.insert(node.next);
		node.next = temp;
		return node;
	}

	let PQ = function () {
		this._head = null;
	};

	PQ.prototype.enqueue = function (priority, value) {
		let node = new Node(priority, value);
		this._head = node.insert(this._head);
	};

	PQ.prototype.dequeue = function () {
		if (this._head === null) {
			throw new Error("empty queue");
		}

		nElements--;
		let first = this._head;
		this._head = this._head.next;
		return { priority: first.priority, value: first.value };
	};

	return PQ;
}();

let queue = new PriorityQueue();
queue.enqueue(1, "foo");
queue.enqueue(3, "bar");
queue.enqueue(2, "baz");

let res = queue.dequeue();
console.log("Dequeued: " + res.value);
AssertEquals(res.value, "bar");
