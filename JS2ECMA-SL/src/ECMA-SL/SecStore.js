const Lattice = require('./syntax/Lattice');
const Store = require('./Store');

class SecStore extends Store{


//SSTO

	getExprLvl(expr){ 
		var vars = expr.getVars();
		var reducer = (accumulator, value) => Lattice.lub(accumulator,this.sto[value]);
		var lvl = vars.reduce(reducer, Lattice.bottom());
		return lvl;
	}

	setVarLvl(x, lvl){
		this.sto[x] = lvl;
	}

	getVarLvl(x){
		return this.sto[x];
	}


	

}

module.exports=SecStore;
