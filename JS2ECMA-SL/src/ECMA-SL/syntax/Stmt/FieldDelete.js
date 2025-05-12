const Expr = require("../Expr/Expr").Expr;
const FieldDeleteLab = require("../Labels/FieldDeleteLab");

function MakeFieldDelete(Stmt){
	 
	class FieldDelete extends Stmt {
	  	constructor(expressionObject, expressionField) {
		    super();
		    this.expressionObject = expressionObject;
		    this.expressionField = expressionField;
		    }

		toString(){
			return ("delete " + this.expressionObject.toString() + "[ " + this.expressionField.toString() + "]");
		}

		toJS(){
			//console.log("FieldDelete toJS");
			var obj_js = this.expressionObject.toJS();
			var field_js = this.expressionField.toJS();
			return {
	      "type": "ExpressionStatement",
	      "expression": {
	        "type": "UnaryExpression",
	        "operator": "delete",
	        "argument": {
	          "type": "MemberExpression",
	          "computed": true,
	          "object": obj_js,
	          "property": field_js
	        },
	        "prefix": true
	      }
	    }
		}

    	interpret(config){
    		//console.log(">FIELD DELETE");
    		config.cont=config.cont.slice(1);
    		var object = this.expressionObject.interpret(config.store).value;
      		var field = this.expressionField.interpret(config.store).value;
    		config.heap.deleteField(object, field);
    	return {config : config, seclabel: new FieldDeleteLab(object, field, this.expressionObject, this.expressionField)};

    	}

	}
	FieldDelete.fromJSON = function(obj) {
		var expr_obj = Expr.fromJSON(obj.obj);
		var expr_field = Expr.fromJSON(obj.field);
		return new FieldDelete(expr_obj,expr_field);

	}
	return FieldDelete;
}

module.exports = MakeFieldDelete;
