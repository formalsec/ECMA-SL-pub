const Stmt = require("./Stmt/Stmt");

class Func {
  constructor(name, params, body) {
    this.name = name;
    this.params = params;
    this.body = body;
  }

  toString() {
    return `function ${this.name} (${this.params}) {\n${this.body.toString()}\n}`;
  }

  toJS(){
    console.log(">>> FUNCTION "+this.name);
    var params = this.params.map((param) => {return {type: "Identifier", name: param}});
    var body_js = this.body.toJS();
    return {
        type: "FunctionDeclaration",
        id: {
          type: "Identifier",
          name: this.name
        },
        params: params,
        body: body_js,
        generator: false,
        expression: false,
        async: false
      }
    }
}

Func.fromJSON = function (obj) { 
  
  var name = obj.name; 
  var params = obj.params;
  var body = Stmt.fromJSON(obj.body);  
  return new Func(name, params, body); 
}

module.exports = Func;
