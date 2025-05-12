const Expr = require("../Expr/Expr").Expr;
const FieldAssignLab = require("../Labels/FieldAssignLab");


function MakeFieldAssign(Stmt){
  class FieldAssign extends Stmt {
    constructor(expressionObject, expressionField, expressionValue) {
      super();
      this.expressionObject = expressionObject;
      this.expressionField = expressionField;
      this.expressionValue = expressionValue;
    }

    toString() {
      return `${this.expressionObject.toString()}[${this.expressionField.toString()}] := ${this.expressionValue.toString()}`;
    }

    toJS(){
      //console.log("FieldAssign toJS");
      var obj_js = this.expressionObject.toJS();
      var field_js = this.expressionField.toJS();
      var expr_js = this.expressionValue.toJS();
      console.log("---------------");
      console.log (obj_js);
      console.log(field_js);
      console.log(expr_js);
      console.log("===============");
      return{
        "type": "ExpressionStatement",
        "expression": {
          "type": "AssignmentExpression",
          "operator": "=",
          "left": {
            "type": "MemberExpression",
            "computed": true,
            "object": obj_js,
            "property": field_js
          },
          "right": expr_js
        }
      }
    }

    interpret(config) {
      //console.log('>FIELD ASSIGN');
      config.cont = config.cont.slice(1) ;
      var object = this.expressionObject.interpret(config.store).value;
      var field = this.expressionField.interpret(config.store).value;
      config.heap.setFieldValue(object, field, this.expressionValue.interpret(config.store));
      return {config : config, seclabel: new FieldAssignLab(object, field, this.expressionObject, this.expressionField, this.expressionValue)};
    }
  }
  FieldAssign.fromJSON = function(obj) {
  	var expr_obj = Expr.fromJSON(obj.obj);
  	var expr_field = Expr.fromJSON(obj.field);
  	var expr_value = Expr.fromJSON(obj.value);
  	return new FieldAssign(expr_obj,expr_field,expr_value);

  }
  return FieldAssign;
}

module.exports = MakeFieldAssign;
