const Expr = require("../Expr/Expr").Expr;
const PrintLab = require("../Labels/PrintLab");

function MakePrint(Stmt){

	class Print extends Stmt {
		constructor(expression){
			super();
			this.expression = expression;
		}

		toString(){
			return ("console.log( "+this.expression.toString()+" )");
		}

		toJS(){
			//console.log("Print toJS");
			var expr_js = this.expression.toJS();
			return {
  			"type": "ExpressionStatement",
  			"expression": {
		        "type": "CallExpression",
		        "callee": {
		          "type": "MemberExpression",
		          "computed": false,
		          "object": {
		            "type": "Identifier",
		            "name": "console"
		          },
		          "property": {
		            "type": "Identifier",
		            "name": "log"
		          }
		        },
		        "arguments": [
		          expr_js
		        ]
		      }
		    }
		}

		interpret(config){
			//console.log(">PRINT");	
			config.cont = config.cont.slice(1);
			var v = this.expression.interpret(config.store);
			if(v != undefined){
				console.log("PRINT> "+ v +"/n");
				return {config : config, seclabel: new PrintLab(this.expression)};
			} else {
				console.log("Undefined Print");
				return {config : config, seclabel: new PrintLab(this.expression)};
			}
		}
	}

	Print.fromJSON = function(obj){
		var expr = Expr.fromJSON(obj.expr);
		return new Print(expr);
	}
	return Print;
}

module.exports = MakePrint;