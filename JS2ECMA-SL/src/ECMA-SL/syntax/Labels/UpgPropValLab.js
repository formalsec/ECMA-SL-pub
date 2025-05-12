const Lattice = require("../Lattice");

class UpgPropValLab{  // Val_lvl

	constructor(location, field, e_o, e_f, lvl){
		this.location = location;
		this.field=field;
		this.e_o=e_o;
		this.e_f = e_f;
		this.lvl=lvl;
	}

	interpret(sec_conf){
		var lev_o = sec_conf.ssto.getExprLvl(this.e_o);
		var lev_f = sec_conf.ssto.getExprLvl(this.e_f);
		var lev_ctx = Lattice.lubn([lev_o, lev_f, sec_conf.pc[0]]);
		var exists = sec_conf.sheap.fieldCheck(this.location, this.field);
		if(exists){
			if(Lattice.leq(lev_ctx, sec_conf.sheap.getFieldValLvl(this.location, this.field))){
				sec_conf.sheap.setFieldValLvl(this.location, this.field, Lattice.lub(this.lvl, lev_ctx));
			} else{
				sec_conf.error = "Illegal P_Val Upgrade";
			}
		} 
		return sec_conf;
	}
}

module.exports = UpgPropValLab;