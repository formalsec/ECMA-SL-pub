
const Oper = require("../Oper");

function MakeBinOptExpr(Expr){
  class BinOptExpr extends Expr {
    constructor(operator, expr_lhs, expr_rhs ) {
      super();
      this.operator = operator;
      this.expr_lhs = expr_lhs;
      this.expr_rhs = expr_rhs;

    }

    toJS(){
      var left_js = this.expr_lhs.toJS();
      var right_js = this.expr_rhs.toJS();
      var oper_js = this.operator.toJS(left_js,right_js);
      return oper_js
    }

    interpret(store){
      //console.log("++BINOPT");
      var v1 = this.expr_lhs.interpret(store);
      var v2 = this.expr_rhs.interpret(store);
      return this.operator.interpret(v1,v2); 
    }
    getVars(){
      var vars = this.expr_lhs.getVars().concat(this.expr_rhs.getVars());
      return vars; 
    }
  }

    BinOptExpr.fromJSON = function(obj){
    	expr_lhs =  Expr.fromJSON(obj.lhs);
    	expr_rhs =  Expr.fromJSON(obj.rhs);
    	oper = Oper.fromJSON(obj.op);
    	return new BinOptExpr(oper,expr_lhs, expr_rhs);
  }

  BinOptExpr.toString = function(){
    return ("("+this.expr_lhs.toString()+" "+this.operator.toString()+" " +this.expr_rhs.toString() + ")");
  }

  return BinOptExpr;
}

module.exports = MakeBinOptExpr;