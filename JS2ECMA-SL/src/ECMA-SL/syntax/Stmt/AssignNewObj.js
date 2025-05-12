const Store = require("../../Store");
const Heap = require("../Heap");
const ValModule =require("../Val/Val");
const LocationVal = ValModule.LocationVal;
const AssignNewObjLab = require("../Labels/AssignNewObjLab");

function MakeAssignNewObj(Stmt){
	class AssignNewObj extends Stmt {
	  constructor(stringvar) {
	    super();
	    this.stringvar = stringvar;
	  }

	  interpret(config){
	  	//console.log(">ASSIGN NEW OBJ");
	  	var obj_name = config.heap.createObject();
	  	config.store.sto[this.stringvar] = new LocationVal(obj_name);
	  	config.cont=config.cont.slice(1);
	  	return {config : config, seclabel: new AssignNewObjLab(this.stringvar, obj_name)};
	  }

	  toJS(){
	  	//console.log("AssignNewObj toJS");
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
	          "type": "ObjectExpression",
	          "properties": []
	        }
	      }
	    }
		}
	}

	AssignNewObj.fromJSON = function(obj) {
		stringvar = obj.lhs;
		return new AssignNewObj(stringvar);

	}

	return AssignNewObj;
}

module.exports = MakeAssignNewObj;
