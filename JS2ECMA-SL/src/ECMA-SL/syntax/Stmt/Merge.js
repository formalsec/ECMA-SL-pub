const MergeLab = require("../Labels/MergeLab");

class Merge {
  constructor() {}

  interpret(config){
  	//console.log(">MERGE");
  	config.cont = config.cont.slice(1);
  	return {config : config, seclabel: new MergeLab()};
  }
}

module.exports = Merge
