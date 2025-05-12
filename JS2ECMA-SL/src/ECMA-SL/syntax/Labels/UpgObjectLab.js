const Lattice = require("../Lattice");

class UpgObjectLab{  // Object_lvl
	constructor(location, e_o, lvl){
		this.location = location;
		this.e_o=e_o;
		this.lvl=lvl;
	}

	interpret(sec_conf){
		var lev_o = sec_conf.ssto.getExprLvl(this.e_o);
		var lev_ctx = Lattice.lub(lev_o, sec_conf.pc[0]);
		var exists = sec_conf.sheap.locationCheck(this.location);
		if(exists){
			console.log(lev_ctx);
			console.log( sec_conf.sheap.getObjectLvl(this.location));
			if(Lattice.leq(lev_ctx, sec_conf.sheap.getObjectLvl(this.location))){
				sec_conf.sheap.setObjectLvl(this.location, Lattice.lub(this.lvl, lev_ctx));
			} else{
				sec_conf.error = "Illegal P_Val Upgrade";
			}
		} 
		return sec_conf;
	}
}

module.exports = UpgObjectLab;

