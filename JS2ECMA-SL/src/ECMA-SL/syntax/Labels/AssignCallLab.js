const SCSFrame = require('../../SCSFrame');
const SecStore = require('../../SecStore');
const Lattice = require("../Lattice");

class AssignCallLab{
	constructor(stringvar, f, args){
		this.stringvar = stringvar;
		this.f = f;
		this.args = args;
	}
	interpret(sec_conf){
		var pc_lvl = sec_conf.pc[0];
		var var_lvl = sec_conf.ssto.getVarLvl(this.stringvar);
		if(!Lattice.leq(pc_lvl, var_lvl)){
				sec_conf.error = "Pc bigger than x in AssignCall";
			}
		sec_conf.scs.push(new SCSFrame(sec_conf.pc, sec_conf.ssto, this.stringvar));
		var lvls = this.args.map(e => sec_conf.ssto.getExprLvl(e));
      	if(this.f != undefined){
       		sec_conf.ssto = new SecStore(this.f.params, lvls);
      	}
		return sec_conf;
	}
}
module.exports = AssignCallLab;