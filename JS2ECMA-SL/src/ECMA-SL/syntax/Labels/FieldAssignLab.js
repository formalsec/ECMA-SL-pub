const Lattice = require('../Lattice');

class FieldAssignLab{
	constructor( object, field, expr_obj, expr_field, expression){
		this.e_o = expr_obj;
		this.e_f = expr_field;
		this.object = object;
		this.field = field;
		this.expression = expression;
	}

	interpret(sec_conf){
		console.log(">FIELD ASSIGN LAB");
		var lev_o = sec_conf.ssto.getExprLvl(this.e_o);
		var lev_f = sec_conf.ssto.getExprLvl(this.e_f);
		var lev_ctx = Lattice.lubn([lev_o, lev_f, sec_conf.pc[0]]);
		var lev_expr = sec_conf.ssto.getExprLvl(this.expression);
		var field_check = sec_conf.sheap.fieldCheck(this.object, this.field);
		if(field_check){
			if(Lattice.leq(lev_ctx,sec_conf.sheap.getFieldValLvl(this.object,this.field))){
				sec_conf.sheap.setFieldValLvl(this.object, this.field, Lattice.lub(lev_expr, lev_ctx));
			} else{
				sec_conf.error = "Illegal Field Assign";
			}
		} else{
			var struct_lvl = sec_conf.sheap.getStructLvl(this.object);
			console.log(struct_lvl);
			console.log(lev_ctx);
			if(struct_lvl != undefined){
				
				if(Lattice.leq(lev_ctx, struct_lvl)){
					console.log(lev_expr);
					console.log(lev_ctx);
					Lattice.lub(lev_expr, lev_ctx)
					sec_conf.sheap.setFieldLvls(this.object, this.field, Lattice.lub(lev_expr, lev_ctx));
					
				} else{
					sec_conf.error = "Illegal Field Assign";
				}
			}else{
				throw Error("Internal Error");
			}
		}

		return sec_conf;
	}

}

module.exports= FieldAssignLab;