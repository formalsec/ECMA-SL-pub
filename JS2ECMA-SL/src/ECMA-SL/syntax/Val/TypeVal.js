function MakeTypeVal(Val){
	class TypeVal extends Val{
  		constructor(value) {
  			super();
    		this.value = value;
	  }

	  toJS(){
    	return {
        "type": "Literal",
        "value": this.value,
        "raw": ""+this.value
    	}
  	}
	}

	 TypeVal.fromJSON = function(value) {
	 	return new TypeVal(value);
	 	
	}
	return TypeVal;
}
module.exports = MakeTypeVal;