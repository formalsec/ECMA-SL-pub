const Lattice = require('../Lattice');

class FieldDeleteLab{
	constructor(object, field, e_o, e_f){
		this.object=object;
		this.field=field;
		this.e_o=e_o;
		this.e_f=e_f; 
	}
	interpret(sec_conf){
		var lev_o = sec_conf.ssto.getExprLvl(this.e_o);
		var lev_f = sec_conf.ssto.getExprLvl(this.e_f);
		var lev_ctx = Lattice.lubn([lev_o, lev_f, sec_conf.pc[0]]);
		var field_check = sec_conf.sheap.fieldCheck(this.object, this.field);
		if (field_check){
			console.log(sec_conf.sheap.getFieldExistsLvl(this.object, this.field));
			if(Lattice.leq(lev_ctx, sec_conf.sheap.getFieldExistsLvl(this.object, this.field))){
				sec_conf.sheap.deleteField(this.object, this.field);
			} else {
				sec_conf.error = "Illegal Field Delete";
			}
		} 
		return sec_conf;
	}

}

module.exports= FieldDeleteLab;
