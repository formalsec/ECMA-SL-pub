const Expr = require("../Expr/Expr").Expr;
const BranchLab = require("../Labels/BranchLab");
const EmptyLab = require("../Labels/EmptyLab");
const Merge = require("./Merge");

function MakeCondition(Stmt){
	class Condition extends Stmt {
		constructor(expr,then_block, else_block){
			super();
			this.expr = expr;
			this.then_block = then_block;
			this.else_block = else_block;
		}
		toString(){
			var else_str = this.else_block ? ("else {\n" + this.else_block.toString() + "\n}") : "";
			return "if(" + this.expr.toString() + ") {\n" +this.then_block.toString() + "\n}" + else_str;
		}

		toJS(){
			//console.log("Condition toJS");
			var expr_js = this.expr.toJS();
			var then_js = this.then_block.toJS();

			var else_js = {
        "type": "BlockStatement",
        "body": []
      };
			if(this.else_block != null){
				else_js = this.else_block.toJS();
			}
			return  {
	      "type": "IfStatement",
	      "test": expr_js,
	      "consequent": then_js,
     	 	"alternate": else_js
		}
	}

		interpret(config){
			//console.log(">CONDITION");
			var v = this.expr.interpret(config.store);
			//Needs to be bool and true
			if(v.value){				
				config.cont = [this.then_block].concat([new Merge()]).concat(config.cont.slice(1));
				return {config : config, seclabel: new BranchLab(this.expr)};
			} else{
				if(this.else_block){
					config.cont = [this.else_block].concat([new Merge()]).concat(config.cont.slice(1));
					return {config : config, seclabel: new BranchLab(this.expr)};
				}
				else{
					config.cont = config.cont.slice(1);
					return {config : config, seclabel: new EmptyLab()};
				}

			}
			
		}
	}

	Condition.fromJSON = function(obj){
		var expr = Expr.fromJSON(obj.expr);
		var then_block = Stmt.fromJSON(obj.then);
		if(obj.else){ 
			var else_block = Stmt.fromJSON(obj.else);
			return new Condition(expr,then_block,else_block);
		} else {
			return new Condition(expr,then_block,null);
		}
	}
	return Condition;
}

module.exports= MakeCondition;