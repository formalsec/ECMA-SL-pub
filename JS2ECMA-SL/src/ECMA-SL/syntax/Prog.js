const Func = require("./Func");

class Prog {
  constructor(funcs) {
    this.funcs = funcs;
  }

  toString(){
  	var funcs_str = this.funcs.map(f => f.toString());
  	return funcs_str.join("\n");
  }

  toJS(){
    var funcs_js = this.funcs.map((func) => func.toJS());
    var main_add = funcs_js.push(
         {
      "type": "ExpressionStatement",
      "expression": {
        "type": "CallExpression",
        "callee": {
          "type": "Identifier",
          "name": "main"
        },
        "arguments": []
      }
    }
        );
    return {
      "type": "Program",
      "body": funcs_js,
      "sourceType": "script"
    }
  }

  getFunc(name){
    return this.funcs.find(func => func.name === name);   
  }
}

Prog.fromJSON = function (obj) {
  var funcs = obj.funcs.map(func => Func.fromJSON(func));
  return new Prog(funcs); 
}

module.exports = Prog;
