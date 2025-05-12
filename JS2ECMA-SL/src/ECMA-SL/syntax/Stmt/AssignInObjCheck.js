const Expr = require("../Expr/Expr").Expr; 
const AssignInObjCheckLab = require("../Labels/AssignInObjCheckLab");
const ValModule = require("../Val/Val");
const PrimitiveVal = ValModule.PrimitiveVal;


function MakeAssignInObjCheck(Stmt){
  class AssignInObjCheck extends Stmt {
    constructor(stringvar, expressionObject, expressionField) {
      super();
      this.expressionObject = expressionObject;
      this.expressionField = expressionField;
      this.stringvar = stringvar;
    }

    interpret(config){
      //console.log(">ASSIGN IN OBJ CHECK");
      config.cont = config.cont.slice(1) ;
      var object = this.expressionObject.interpret(config.store).value;
      var field = this.expressionField.interpret(config.store).value;
      var v = config.heap.fieldCheck(object, field);
      config.store.setValue(this.stringvar, new PrimitiveVal(v));
      return {config : config, seclabel: new AssignInObjCheckLab(this.stringvar, field, object ,this.expressionField ,this.expressionObject)};
    }

    toJS(){
      //console.log("AssignInObjCheck toJS");
      var obj_js = this.expressionObject.toJS();
      var field_js = this.expressionField.toJS();
      return {
        "type": "ExpressionStatement",
        "expression": {
          "type": "AssignmentExpression",
          "operator": "=",
          "left": {
            "type": "Identifier",
            "name": this.stringvar
          },
          "right": {
            "type": "CallExpression",
            "callee": {
              "type": "MemberExpression",
              "computed": false,
              "object": obj_js,
              "property": {
                "type": "Identifier",
                "name": "hasOwnProperty"
              }
            },
            "arguments": [
              field_js
            ]
          }
        }
      }
    }

   
  }
  AssignInObjCheck.fromJSON = function(obj) {
  	var expr_obj = Expr.fromJSON(obj.obj);
  	var expr_field = Expr.fromJSON(obj.field);
  	var stringvar = obj.lhs;
  	return new AssignInObjCheck(stringvar,expr_obj,expr_field);

  }
  return AssignInObjCheck;
}
module.exports = MakeAssignInObjCheck;
