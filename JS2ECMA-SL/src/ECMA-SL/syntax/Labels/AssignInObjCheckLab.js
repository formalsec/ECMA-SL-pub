const Lattice = require("../Lattice");

class AssignInObjCheckLab{
	constructor(stringvar, field, object, e_f, e_o){
		this.field = field;
		this.object = object;
		this.stringvar = stringvar;
		this.e_f = e_f;
		this.e_o = e_o;
	}
	interpret(sec_conf){
		console.log(">ASSIGNINOBJ LAB");
		var field_lvl=sec_conf.ssto.getExprLvl(this.e_f);
		var object_lvl=sec_conf.ssto.getExprLvl(this.e_o);
		var pc_lvl = sec_conf.pc[0];
		var ctx_lvl = Lattice.lubn([field_lvl, object_lvl, pc_lvl]);
		var var_lvl = sec_conf.ssto.getVarLvl(this.stringvar);
		var struct_lvl =sec_conf.sheap.getStructLvl(this.object);
		var exists = sec_conf.sheap.fieldCheck(this.object, this.field);
		if(exists){
			if(var_lvl != undefined){
				// Some (lvl, _ ), Some x_lvl, _
				if(Lattice.leq(ctx_lvl, var_lvl)){
					sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(ctx_lvl, sec_conf.sheap.getFieldExistsLvl(this.object, this.field)));
				} else {
					sec_conf.error = 'Illegal Assignment';
				}
			} else{
				 // Some (lvl, _ ), None, _   
				 	sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(ctx_lvl, sec_conf.sheap.getFieldExistsLvl(this.object, this.field)));
				}
		} else {
			 // None, Some x_lvl, Some struct_lvl
			if(var_lvl != undefined){
				if(struct_lvl != undefined){  
					if(Lattice.leq(ctx_lvl, var_lvl)){
						sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(ctx_lvl, struct_lvl));
					} else {
						sec_conf.error = 'Illegal Assignment';
					}
				} else {
					//NO STRUCTLVL
					raise ("Internal Error");
				}
			} else {
				//None, None, Some struct_lvl
				if(struct_lvl != undefined){ 
					sec_conf.ssto.setVarLvl(this.stringvar, Lattice.lub(ctx_lvl, struct_lvl));
				} else {
					raise ("Internal Error");
				}
			}

		}	
		return sec_conf;
	}
	
}

module.exports = AssignInObjCheckLab;

/*
 | AssignInObjCheckLab (x, f, o, e_f, e_o) ->
    let lvl_pc= check_pc pc in
    let ef_lvl= expr_lvl ssto e_f in
    let eo_lvl= expr_lvl ssto e_o in
    let ctx_lvl = SL.lubn ([lvl_pc ; ef_lvl ; eo_lvl]) in
    let x_lvl = SecStore.get_safe ssto x in 
    let struct_lvl = SecHeap.get_struct_lvl sheap o in
    (match SecHeap.get_field sheap o f, x_lvl, struct_lvl with
      | Some (lvl, _ ), Some x_lvl, _ ->
          if (SL.leq ctx_lvl x_lvl) then (
            SecStore.set ssto x (SL.lub lvl ctx_lvl);
            MReturn (scs, sheap, ssto, pc))
          else MFail((scs,sheap,ssto,pc), "Illegal Assignment ")
      | Some (lvl, _ ), None, _ ->  
          SecStore.set ssto x (SL.lub lvl ctx_lvl);
          MReturn (scs, sheap, ssto, pc)
      | None, Some x_lvl, Some struct_lvl -> 
          if (SL.leq ctx_lvl x_lvl) then (
            SecStore.set ssto x (SL.lub ctx_lvl struct_lvl);
            MReturn (scs, sheap, ssto, pc))
          else MFail((scs,sheap,ssto,pc), "Illegal Assignment ")
      | None, None, Some struct_lvl ->
          SecStore.set ssto x (SL.lub ctx_lvl struct_lvl);
          MReturn (scs, sheap, ssto, pc)
      | _ -> raise (Except "Internal Error"))


*/