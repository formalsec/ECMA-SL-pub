function MakePrimitiveVal(Val){
  class PrimitiveVal extends Val {
    constructor(value) {
    	super();
      this.value = value;
    }

    toString() {
      if ((typeof this.value) === "string") {
        return `"${this.value.split('\\').join('\\\\')
                             .split('"').join('\\"')}"`
      }

      if (this.value !== this.value) {
        return "nan";
      }

      if (((typeof this.value) === "number") &&
          (this.value === Math.floor(this.value)) &&
          (this.value < 1000000000000000000000)){
        return "" + this.value + "."
      }

      return ("" + this.value);
    }

    toJS(){
      return {
        "type": "Literal",
        "value": this.value,
        "raw": "\""+this.value+"\""
      }
    }


  }

  PrimitiveVal.fromJSON = function(value) {
   	return new PrimitiveVal(value);
  }
  return PrimitiveVal;
}

module.exports = MakePrimitiveVal;
