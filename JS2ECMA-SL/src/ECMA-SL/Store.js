const Lattice = require('./syntax/Lattice');
class Store{

constructor(params, vs){
	//console.log(params);
	var result = {};
	params.forEach((param, i) => result[param]= vs[i]);
	this.sto = result;
	}
	
//STO
	getValues(vars_arr){
		var reducer = (accumulator, value) => Lattice.lub(accumulator,value);
		var value = vars_arr.map(vs => vs.reduce(reducer, 0));
		return value;
	}

	setValue(stringvar, value){
		this.sto[stringvar] = value;

	}
}

module.exports=Store;
