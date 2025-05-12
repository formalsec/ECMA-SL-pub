function MakeVoidVal(Val){

	class VoidVal extends Val{
	  constructor() {
	  	super();
	  }

	  toJS(){
      return {
        "type": "Literal",
        "value": "void",
        "raw": "void"
      }
    }
	}

	 VoidVal.fromJSON = function() {
	 	return new VoidVal();
	}

	return VoidVal;
}
module.exports = MakeVoidVal;