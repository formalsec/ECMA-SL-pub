class NewLab{
	constructor(stringvar, location){
		this.stringvar = stringvar;
		this.location = location;
	}

	interpret(sec_conf){
		console.log(">NEW OBJ LAB");
		var pc_lvl = sec_conf.pc[0];
		sec_conf.sheap.createObject(this.location, pc_lvl);
		sec_conf.ssto.setVarLvl(this.stringvar, pc_lvl);
		return sec_conf;
	}
}

module.exports= NewLab;