
function MakeLocationVal(Val){
	class LocationVal extends Val{
	  constructor(value) {
	  	super();
	    this.value = value;
	  }
	}

	LocationVal.fromJSON = function(value) {
	 return new LocationVal(value);
	}
	return LocationVal;
}

 module.exports = MakeLocationVal;