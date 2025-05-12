const Expr = require("../Expr/Expr").Expr;
const FieldLookupLab = require("../Labels/FieldLookupLab");

function MakeFieldLookup(Stmt){
  
  class FieldLookup extends Stmt {
    constructor(stringvar, expressionObject, expressionField) {
      super();
      this.expressionObject = expressionObject;
      this.expressionField = expressionField;
      this.stringvar = stringvar;
    }

    toString(){
      return (this.stringvar + " = "+ this.expressionObject.toString() + "["+ this.expressionField+"]");
    }

    toJS(){
      //console.log("FieldLookup toJS");
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
            "type": "MemberExpression",
            "computed": true,
            "object": obj_js,
            "property": field_js
          }
        }
      }
    }

    interpret(config){
      //console.log(">FIELD LOOKUP");
      config.cont=config.cont.slice(1);
      var object = this.expressionObject.interpret(config.store).value;
      var field = this.expressionField.interpret(config.store).value;
      config.store.sto[this.stringvar] = config.heap.getField(object, field);
      
     return {config : config, seclabel: new FieldLookupLab(this.stringvar, object, field, this.expressionObject, this.expressionField)};
    }

   
  }
  FieldLookup.fromJSON = function(obj) {
  	var expr_obj = Expr.fromJSON(obj.obj);
  	var expr_field = Expr.fromJSON(obj.field);
  	var stringvar = obj.lhs;
  	return new FieldLookup(stringvar,expr_obj,expr_field);

  }
  return FieldLookup;
}

module.exports = MakeFieldLookup;
