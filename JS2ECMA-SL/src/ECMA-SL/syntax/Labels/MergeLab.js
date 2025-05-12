class MergeLab{

	interpret(sec_conf){
		sec_conf.pc = sec_conf.pc.slice(1);
		return sec_conf;
	}
}

module.exports= MergeLab;