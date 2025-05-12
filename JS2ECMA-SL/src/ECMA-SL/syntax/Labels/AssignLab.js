const Lattice = require("../Lattice");

class AssingLab{
	constructor(stringvar, expr){
		this.stringvar = stringvar;
		this.expr = expr;
	}
	interpret(sec_conf){
		console.log(">ASSIGN LAB");
		var expr_lvl=sec_conf.ssto.getExprLvl(this.expr);
		var pc_lvl = sec_conf.pc[0];
		var var_lvl = sec_conf.ssto.getVarLvl(this.stringvar);
		if(var_lvl != undefined){
			if(Lattice.leq(pc_lvl, var_lvl)){
				sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(pc_lvl, expr_lvl));
			}else{
				sec_conf.error = "Illegal Assignment";
			}
		}
		else{

			sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(pc_lvl, expr_lvl));
		}
		return sec_conf;

	}
	
}

module.exports = AssingLab;