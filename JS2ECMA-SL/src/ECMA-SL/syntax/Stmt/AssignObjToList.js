	const Expr = require("../Expr/Expr").Expr; 
	const EmptyLab = require("../Labels/EmptyLab");

function MakeAssignObjToList(Stmt){
	class AssignObjToList extends Stmt {
	  constructor(stringvar, expressionObject) {
	    super();
	    this.expressionObject = expressionObject;
	    this.stringvar = stringvar;
	  }

	 
	}
	AssignObjToList.fromJSON = function(obj) {
		var expr_obj = Expr.fromJSON(obj.obj);
		var stringvar = obj.lhs;
		return new AssignInObjCheck(stringvar,expr_obj);

	}
	return AssignObjToList;
}
module.exports = MakeAssignObjToList;
