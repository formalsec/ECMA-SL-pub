const ValModule = require("../Val/Val");
const Val = ValModule.Val;
const SymbolVal = ValModule.SymbolVal;

function MakeVarExpr(Expr){

	class VarExpr extends Expr {
	  constructor(varStr) {
	    super();
	    this.variable = varStr;
	  }

	  toString() {
	    return this.variable;
	  }

	  toJS(){
	  	return {
        "type": "Identifier",
        "name": this.variable
	    }
	  }

	  interpret(store){
	  	//console.log("++ VAR EXPR");
	  	var val = store.sto[this.variable];
	    if (val == undefined)  return new SymbolVal("undefined");
	    else
	  	return val;
	  }
	  getVars(){
	  	return [this.variable];
	  }
	}

	VarExpr.fromJSON = function(obj) {
		return new VarExpr(obj.name);
	}
	return VarExpr;
}
module.exports = MakeVarExpr;
