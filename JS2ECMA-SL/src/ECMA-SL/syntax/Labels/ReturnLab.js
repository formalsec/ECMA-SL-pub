const UpgVarLab = require("./UpgVarLab");
const Lattice = require("../Lattice");
class ReturnLab{
	constructor(expr){
		this.expr=expr;
	}
	interpret(sec_conf){
		var frame = sec_conf.scs.pop();
  		var return_lvl = sec_conf.ssto.getExprLvl(this.expr);
  		sec_conf.ssto = frame.ssto;
  		sec_conf.pc = frame.pc;
  		//nsu
  		var lab = new UpgVarLab(frame.stringvar, Lattice.lub(return_lvl, sec_conf.pc[0]));
		return lab.interpret(sec_conf);
	}
}

module.exports= ReturnLab;