const Lattice = require('../Lattice');
class FieldLookupLab{
	constructor(stringvar, object, field, e_o, e_f){
		this.stringvar=stringvar;
		this.object=object;
		this.field=field;
		this.e_o = e_o;
		this.e_f = e_f;
	}
	interpret(sec_conf){
		console.log("FIELD LOOKUP LAB");
		var lev_o = sec_conf.ssto.getExprLvl(this.e_o);
		var lev_f = sec_conf.ssto.getExprLvl(this.e_f);
		var lev_ctx = Lattice.lubn([lev_o, lev_f, sec_conf.pc[0]]);
		var lev_var = sec_conf.ssto.getVarLvl(this.stringvar);
		if(lev_var == undefined){
			lev_var = lev_ctx;
		} 
		if(Lattice.leq(lev_ctx, lev_var)){
			var field_check = sec_conf.sheap.fieldCheck(this.object, this.field);
			if(field_check){
				var lub = Lattice.lub(lev_ctx, sec_conf.sheap.getFieldValLvl(this.object, this.field));
				sec_conf.ssto[this.stringvar] = lub;
			}

		} else{
			sec_conf.error = "Illegal Field Lookup";
		}

		return sec_conf;
	}
}

module.exports= FieldLookupLab;
