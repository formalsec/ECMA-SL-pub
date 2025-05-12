class Expr {
}

const Val = require("./ValExpr")(Expr);
const Var = require("./VarExpr")(Expr);
const UnOpt = require("./UnOptExpr")(Expr);
const BinOpt = require("./BinOptExpr")(Expr);
const NOpt = require("./NOptExpr")(Expr);
const NewObj = require("./NewObj")(Expr);


Expr.fromJSON = function(obj) {
	switch (obj.type) {
       case "value": return Val.fromJSON(obj);
       case "var": return Var.fromJSON(obj);
       case "unop": return UnOpt.fromJSON(obj);
       case "binop": return BinOpt.fromJSON(obj);
       case "nop": return NOpt.fromJSON(obj);
       case "newObj": return NewObj.fromJSON(obj);
       default: throw new Error("Unsupported expression: " + obj.type);
   }
}



module.exports = {
	Expr,
	Val,
	Var,
	UnOpt,
	BinOpt,
  NOpt,
  NewObj
	};
