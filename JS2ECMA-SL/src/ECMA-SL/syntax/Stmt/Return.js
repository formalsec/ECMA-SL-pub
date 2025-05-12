const Expr = require("../Expr/Expr").Expr;
const ReturnLab = require("../Labels/ReturnLab");
const EmptyLab = require("../Labels/EmptyLab");

function MakeReturn(Stmt){
	
	class Return extends Stmt {
	  constructor(expression) {
	    super();
	    this.expression = expression
	  }

	  toString() {
	    return `return ${this.expression.toString()}`
	  }

	  toJS(){
	  	//console.log("Return toJS");
	  	var expr_ast = this.expression.toJS();
	  	return {
            "type": "ReturnStatement",
            "argument": expr_ast
          }
	  }

	  interpret(config){
	  	//console.log(">RETURN");

	  	if (config.cs.length > 1){
	  		var frame = config.cs.pop();
	  		var return_value = this.expression.interpret(config.store);
	  		//console.log("RETURN:\n");
	  		config.store = frame.store;
	  		config.cont = frame.cont;
	  		//console.log("config.cont = "+ config.cont);
	  		config.store.sto[frame.stringVar]=return_value;
	  		//console.log("*********** RETURN DEBUG *********");
	        //console.log(return_value);
	        //console.log("***************************");
	  		return {config : config, seclabel: new ReturnLab(this.expression)};
	  	}
	  	else{
	  		config.cont=[];
	  		config.final_return = this.expression.interpret(config.store);
	  		return {config : config, seclabel: new EmptyLab()};
	  	}
	  	
	  	
	  }
	}
	Return.fromJSON = function(obj) {
		var expr = Expr.fromJSON(obj.expr);
		return new Return(expr);
	}
	return Return;
}

module.exports = MakeReturn
