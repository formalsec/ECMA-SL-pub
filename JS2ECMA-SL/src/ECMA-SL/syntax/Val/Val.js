class Val{

  constructor(value) {
    this.value = value;
  }

  toString() {
    return this.value.toString();
  }
}

var PrimitiveVal = require("./PrimitiveVal")(Val);
var LocationVal = require("./LocationVal")(Val);
var ListVal = require("./ListVal")(Val);
var TypeVal = require("./TypeVal")(Val);
var VoidVal = require("./VoidVal")(Val);
var SymbolVal = require("./SymbolVal")(Val);



Val.Str = class {
  constructor(value) {
    this.value = value;
  }

  toString() {
    if (this.value.startsWith('"')) {
      return `"${this.value.split('"').join('\\"')}"`;
    }
    return `"${this.value}"`;
  }
};


Val.fromJSON = function(obj){
  console.log(obj);
  switch(obj.type){
    case "float":  
    case "int":
    case "null":
    case "boolean":
    case "string": return PrimitiveVal.fromJSON(obj.value);
    case "location": return LocationVal.fromJSON(obj.value);
    case "list": return ListVal.fromJSON(obj.value);
    case "type": return TypeVal.fromJSON(obj.value);
    case "void": return VoidVal.fromJSON();
    case "symbol": return SymbolVal.fromJSON(obj.value);
    default : throw new Error("Unsupported value: "+obj.type);
  }
  
}

module.exports = {
  PrimitiveVal,
  ListVal,
  LocationVal,
  PrimitiveVal,
  SymbolVal,
  TypeVal,
  Val,
  VoidVal
};
