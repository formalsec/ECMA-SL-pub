
const Expr = require("../Expr/Expr").Expr;
const Condition = require("./Condition")(Expr);
const Block = require("./Block")(Expr);
const EmptyLab = require("../Labels/EmptyLab");

function MakeLoop(Stmt){
	
	class Loop extends Stmt {
		  constructor(expr, block) {
		    super();
		    this.expr = expr;
		    this.block = block;
		}

		toString(){
			return ("while ("+this.expr.toString() +")\n{"+ this.block.toString()+"\n}");
		}

		toJS(){
			//console.log("Loop toJS");
			var expr_js = this.expr.toJS();
			var block_js = this.block.toJS();
			return {
	      "type": "WhileStatement",
	      "test": expr_js,
	      "body": block_js
	    }
		}

		interpret(config){
			//console.log(">LOOP");
			config.cont=config.cont.slice(1);
			var result = [new Condition(this.expr, new Block([this.block].concat([new Loop(this.expr,this.block)])),null)];
			config.cont= result.concat(config.cont);
			return {config : config, seclabel: new EmptyLab()};
		}

	}

	Loop.fromJSON = function(obj){
		var expr = Expr.fromJSON(obj.expr);
		var block = Stmt.fromJSON(obj.do);
		return new Loop(expr,block) 
	}
	return Loop;
}
module.exports = MakeLoop;
