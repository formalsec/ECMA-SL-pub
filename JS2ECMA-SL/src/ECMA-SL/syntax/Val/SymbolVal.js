function MakeSymbolVal(Val){
	class SymbolVal extends Val{
	  constructor(value) {
	  	super();
	    this.value = value;
	  }

	  toJS(){
	  	if(this.value === "undefined"){
	  		return {
	        "type": "Identifier",
	        "name": "undefined",
	      }
	  	} else {
	  		throw "Unsupported Symbol " + this.value+ " (SymbolVal)";
	  	}
	      
    }

    toString() {
      return "'" + this.value
    }
	}

	 SymbolVal.fromJSON = function(value) {
	 	return new SymbolVal(value);
	}
	return SymbolVal;
}
module.exports = MakeSymbolVal;
