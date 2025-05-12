const Lattice = require("../Lattice");

class UpgVarLab{
	constructor(stringvar, lev){
		this.stringvar = stringvar;
		this.lev = lev;
		
	}


	interpret(sec_conf){
		console.log("You are in UpgVarLab with "+ this.stringvar+ " " +this.lev);

		var pc_lvl = sec_conf.pc[0];
		var var_lvl = sec_conf.ssto.getVarLvl(this.stringvar);
		if(this.lev != undefined){
			if(Lattice.leq(pc_lvl, var_lvl)){
				sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(this.lev, pc_lvl));
				console.log("SECSTORE = "+ this.stringvar +" <-"+ Lattice.lub(this.lev, pc_lvl));
			} else {
				sec_conf.error = "Illegal UpgVarLab";
			}
		} else{
			sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(this.lev, pc_lvl));
			console.log("SECSTORE = "+ this.stringvar +" <-"+ Lattice.lub(this.lev, pc_lvl));
		}
		return sec_conf;
	}
}
module.exports = UpgVarLab;

