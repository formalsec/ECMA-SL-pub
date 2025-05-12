const Lattice = require("../Lattice");

class BranchLab{
	constructor(expr){
		this.expr=expr;
	}
	
	interpret(sec_conf){
		console.log(">BRANCH LAB");
		var expr_lvl = sec_conf.ssto.getExprLvl(this.expr);
		var pc_lvl = sec_conf.pc[0];
		sec_conf.pc = [Lattice.lub(pc_lvl,expr_lvl)].concat(sec_conf.pc);
		return sec_conf;
	}
	
}
module.exports= BranchLab;