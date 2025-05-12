const UpgVarLab = require("./Labels/UpgVarLab");
const UpgPropExistsLab = require("./Labels/UpgPropExistsLab");
const UpgPropValLab = require("./Labels/UpgPropValLab");
const UpgObjectLab = require("./Labels/UpgObjectLab");
const UpgStructLab = require("./Labels/UpgStructLab");
const Lattice = require("./Lattice");

class Interceptor{

}

Interceptor.search = function(func_name, vs, exprs){
	//console.log(">INTERCEPTOR");
	//console.log(vs);
	//Arg number check for each
		switch(func_name){
		case "upgVar": return new UpgVarLab(vs[0], Lattice.parseLvl(vs[1]));
		case "upgObject": return new UpgObjectLab(vs[0], exprs[0], Lattice.parseLvl(vs[1]));
		case "upgStruct": return new UpgStructLab(vs[0], exprs[0], Lattice.parseLvl(vs[1]));
		case "upgPropExists": return new UpgPropExistsLab(vs[0], vs[1], exprs[0], exprs[1], Lattice.parseLvl(vs[2]));
		case "upgPropVal": return new UpgPropValLab(vs[0], vs[1], exprs[0], exprs[1], Lattice.parseLvl(vs[2]));
		default: throw new Error("Unkown function");
	}
}

module.exports = Interceptor; 