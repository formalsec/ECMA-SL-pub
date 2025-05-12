function MakeListVal(Val){
	class ListVal extends Val{
	  constructor(list) {
	  	super();
	    this.list = list;
	  }
	  
	  getMember(index){
	  	return this.list[index];
	  }

	  getTail(){
	  	return new ListVal(this.list.slice(1));
	  }

	  toJS(){
	  	var list_js = this.list.map((element) => element.toJS());
	  	return list_js;
	  }
	}

	ListVal.fromJSON = function(list) {
		return new ListVal(list);
	}

	ListVal.fromJSON = function() {
		return "LISTTTT";
	}
	return ListVal;
}
module.exports = MakeListVal;