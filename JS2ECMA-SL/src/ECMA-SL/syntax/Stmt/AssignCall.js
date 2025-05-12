const ExprModule = require("../Expr/Expr"); 
const Store = require("../../Store");
const CsFrame = require("../../CsFrame");
const AssignCallLab = require("../Labels/AssignCallLab");
const Interceptor = require("../Interceptor");
const ValModule = require("../Val/Val");
const ValExpr = ExprModule.Val;
const Expr = ExprModule.Expr;
const PrimitiveVal = ValModule.PrimitiveVal;

function MakeAssignCall(Stmt){
  class AssignCall extends Stmt {
    constructor(stringvar, func, args) {
      super();
      this.stringvar = stringvar;
      this.func = func;
      this.args = args;
      
    }
    toString(){
      var args_str = this.args.map(f => f.toString()); 
      return this.stringvar + " := " + this.func.toString() + "( " + args_str.join(", ") + " )";
    }

    toJS(){
      //console.log("AssignCall " + this.func+ " toJS");
      var args_js = this.args.map((arg) => arg.toJS());
      //console.log(">>>DEBUG");
      //console.log(JSON.stringify(this.func));
      var func; 
      //console.log(this.func instanceof ValExpr);
      //console.log(this.func.constructor);
      if((this.func instanceof ValExpr) && (this.func.value instanceof PrimitiveVal) && ((typeof this.func.value.value) === "string")){
        //console.log("#AssignCall IF #");
        //console.log(this.func);
        func = {
          "type": "Identifier",
          "name": this.func.value.value
        }
      } else {
        func = this.func.toJS(); 
      }
      
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
            "callee": func,
            "arguments": args_js
          }
        }
      }
    }

    interpret(config){
      //console.log(">ASSIGN CALL");
      config.cont=config.cont.slice(1);
      var func_name = this.func.interpret(config.store);
      var f = config.prog.getFunc(func_name.value);
      var vs = this.args.map(e => e.interpret(config.store));
      
      if(f){
        var new_store = new Store(f.params, vs);
        config.cs.push(new CsFrame(this.stringvar, config.cont, config.store));
        config.store = new_store;
        config.cont = [f.body];
        
      }else{
        //interceptor
        var label = Interceptor.search(func_name.value, vs.map(v => v.value), this.args);
        if(label != undefined)
          return {config : config, seclabel: label};
          


      }
      
      return {config : config, seclabel: new AssignCallLab(this.stringvar, f, this.args)};
    }

    
  }
  AssignCall.fromJSON = function(obj) {
  	var stringvar = obj.lhs;
  	var func = Expr.fromJSON(obj.func);
  	var args = obj.args.map(Expr.fromJSON);
  	return new AssignCall(stringvar,func,args);

  }
  return AssignCall;
}

module.exports = MakeAssignCall;
