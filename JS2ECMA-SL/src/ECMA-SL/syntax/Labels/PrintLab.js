const Lattice = require("../Lattice");

class PrintLab{
	constructor(expr){
		this.expr = expr;
	}
	interpret(sec_conf){
		console.log(">PRINTLAB");
		var expr_lvl=sec_conf.ssto.getExprLvl(this.expr);
		var pc_lvl = sec_conf.pc[0];
		if(!Lattice.leq(pc_lvl, expr_lvl)){
			sec_conf.error = "Illegal Print";
		}
		
		return sec_conf;

	}
	
}

module.exports = PrintLab;