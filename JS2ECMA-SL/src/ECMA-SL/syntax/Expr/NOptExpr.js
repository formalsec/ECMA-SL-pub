const Oper = require("../Oper");

function MakeNOptExpr(Expr){
  class NOptExpr extends Expr {
    constructor(n_aryOperator, expressionsList = []) {
      super();
      this.n_aryOperator = n_aryOperator;
      this.expressionsList = expressionsList;
    }

    toString() {
      return this.n_aryOperator.toString(
        this.expressionsList.map((expr) => expr.toString())
      );
    }

    toJS(){
      //console.log("=============DEBUG=============");
      //console.log(this.n_aryOperator);
     // console.log(this.expressionsList);
      //console.log("===============================");
      return this.n_aryOperator.toJS(this.expressionsList);
    }

    interpret(store){
      //console.log("++ NOPT");
      var v_list = this.expressionsList.map((expr) => expr.interpret(store));
      return this.n_aryOperator.interpret(v_list);
    }
    getVars(){
      var vars = this.expressionsList.map(getVars());
      return vars;
    }
  }

  NOptExpr.fromJSON = function(obj){
    args = obj.args.map(Expr.fromJSON);
    oper = Oper.fromJSON(obj.op);
    return new NOptExpr(oper,args);
  }

  NOptExpr.ListExpr = class {
    toString(elements = []) {
      // Array.prototype.join concatenates the empty string when null or undefined appear.
      return `[ ${elements
        .map((elem) => {
          if (elem === null || elem === undefined) {
            return String(elem);
          }
          return elem;
        })
        .join(", ")} ]`;
    }
  }

  return NOptExpr;
}

module.exports = MakeNOptExpr;
