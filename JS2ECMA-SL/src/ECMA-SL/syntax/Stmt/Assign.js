const Expr = require("../Expr/Expr").Expr; 
const AssignLab = require("../Labels/AssignLab");

function MakeAssign(Stmt){

  class Assign extends Stmt {
    constructor(variable, expression) {
      super();
      this.variable = variable;
      this.expression = expression;
    }

    toString() {
      return `${this.variable.toString()} := ${this.expression.toString()}`;
    }

    toJS(){
      var expr_js = this.expression.toJS();
      return {
        "type": "ExpressionStatement",
        "expression": {
          "type": "AssignmentExpression",
          "operator": "=",
          "left": {
            "type": "Identifier",
            "name": this.variable
          },
          "right": expr_js
        }
      }
    }

    interpret(config){
      //console.log(">ASSIGN")
      config.cont=config.cont.slice(1);
      var v = this.expression.interpret(config.store);
      config.store.setValue(this.variable, v);
      return {config : config, seclabel: new AssignLab(this.variable, this.expression)};
    }
  }

  Assign.fromJSON = function(obj) {
    var var_name = obj.lhs;
    var expr = Expr.fromJSON(obj.rhs); 
    return new Assign(var_name, expr);
  }
  return Assign;
}

module.exports = MakeAssign;
