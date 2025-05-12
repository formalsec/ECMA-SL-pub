
const ValModule = require("./Val/Val");
const Val = ValModule.Val;
const PrimitiveVal = ValModule.PrimitiveVal;
const ListVal = ValModule.ListVal;

var binary_dictionary = {
  "Plus": "+", 
  "Minus": "-",
  "Times": "*",
  "Div": "/",
  "Equal": "==",
  "Gt": ">",
  "Lt": "<",
  "Egt": ">=",
  "Elt": "<=",
  "FloatToString": "+"};

var unary_dictionary = {
  "Neg" : "-", 
  "Not" : "!",
  "Typeof" : "typeof",
  "Sconcat": "+"
}
  var logical_dictionary = {
    "Log_And": "&&",
    "Log_Or": "||"
  };
  
  var function_dictionary = {
    "InObj": "hasOwnProperty",
    "Lconcat": "concat",
    "Ladd": "push",
    "InList": "includes",
    "Tail": "slice",
    "IntToFloat": "toFixed"
  }; 
  var nary_opr = {
    "NAry_And": "&&",
    "NAry_Or": "||"
  }

class Oper{
  constructor(operator, type) {
    this.operator = operator;
    this.type = type;
  }

  interpret(val1,val2){
    //console.log("==== "+ this.operator);
  	switch(this.operator){
  		//BinOpt
  		case "Plus":  return new PrimitiveVal(val1.value + val2.value);
  		case "Minus": return new PrimitiveVal(val1.value + val2.value);
  		case "Times": return new PrimitiveVal(val1.value * val2.value);
  		case "Div": return new PrimitiveVal(val1.value - val2.value);
  		case "Equal": return new PrimitiveVal(val1.value === val2.value); //Check
  		case "Gt": return new PrimitiveVal(val1.value > val2.value); //Check
  		case "Lt": return new PrimitiveVal(val1.value < val2.value); //Check
  		case "Egt": return new PrimitiveVal(val1.value >= val2.value); //Check
  		case "Elt": return new PrimitiveVal(val1.value <= val2.value); //Check
  		case "Log_And": return new PrimitiveVal(val1.value && val2.value); //Check
  		case "Log_Or": return new PrimitiveVal(val1.value || val2.value); //Check
  		case "InObj": return new PrimitiveVal(true); //TODO //Extended ECMA-SL
  		case "InList": return new PrimitiveVal(val1.value.includes(val2.value));
  		case "Lnth": return val1.list[val2.value];//TODO
  		case "Ladd":return new PrimitiveVal(true);//TODO
  		case "Lconcat": return new ListVal(val1.value.concat(val2.value))
  		//UnOpt
  		case "Neg": return new PrimitiveVal(-val1.value);
  		case "Not": return new PrimitiveVal(!(val1.value));
  		case "Typeof": return new PrimitiveVal(typeof val1.value);
  		case "ListLen": return new PrimitiveVal(val1.list.length);
  		case "Head": return val1.getMember(0);
  		case "Tail": return val1.getTail();
  		case "IntToFloat": return new PrimitiveVal(0.0 + val1.value);
  		case "FloatToString": return new PrimitiveVal(String.valueOf(val1.value));
  		case "ObjToList": return new ListVal([]);//TODO
  		//NOpt
  		case "ListExpr":  return new ListVal(val1);
  		case "NAry_And":  var reducer = (accumulator, value) => accumulator && value;
  						          return new PrimitiveVal(val1.reduce(reducer, true));
  		case "NAry_Or": var reducer = (accumulator, value) => accumulator || value; 
  						        return new PrimitiveVal(val1.reduce(reducer, false));
      case "Sconcat": var reducer = (accumulator, value) => accumulator + value.value;
                      return new PrimitiveVal(val1.list.reduce(reducer,""));
  		default: throw new Error("Unsupported Argument"+ this.operator)
  	}
  }

  
  memberExpression(e1,e2){
    return {
          "type": "MemberExpression",
          "computed": true,
          "object": e1,
          "property": e2
        };
  }

  callExpression(e1,e2){
    return {
      "type": "CallExpression",
      "callee": {
        "type": "MemberExpression",
        "computed": false,
        "object": e1,
        "property": {
          "type": "Identifier",
          "name": function_dictionary[this.operator]
        }
      },
      "arguments": [
        e2
      ]
    };
  }

  binaryExpression(e1,e2){
    return {
      "type": "BinaryExpression",
      "operator": binary_dictionary[this.operator],
      "left": e1,
      "right": e2
    };
  }

  toJS(e1, e2 = null){
    switch(this.operator){
      //BinOpt
      case "Plus":  
      case "Minus": 
      case "Times": 
      case "Div":
      case "Equal": 
      case "Gt": 
      case "Lt": 
      case "Egt": 
      case "Elt": 
        return this.binaryExpression(e1,e2);
      //LOGICAL OPERATIONS (NOT A BINARY EXPRESSION)
      case "Log_And":
      case "Log_Or": 
        return {
          "type": "LogicalExpression",
          "operator": logical_dictionary[this.operator],
          "left": e1,
          "right": e2
        }; 

      //FUNCTIONS
      case "Tail":
        e2 ={
              "type": "Literal",
              "value": 1,
              "raw": "1"
            };
        return this.callExpression(e1,e2);
      case "IntToFloat":
        e2 = {
              "type": "Literal",
              "value": 2,
              "raw": "2"
            };
        return this.callExpression(e1,e2);
      case "InObj": 
      case "Lconcat": 
      case "Ladd":
      case "InList":
        return this.callExpression(e1,e2);

      case "Lnth": 
        return this.memberExpression(e1,e2);
      //UnOpt
      case "Neg": 
      case "Not": 
      case "Typeof": 
        return {
          "type": "UnaryExpression",
          "operator": unary_dictionary[this.operator],
          "argument": e1,
          "prefix": true
        }
      case "Sconcat":
      //console.log(e1);
        var oper_js = unary_dictionary[this.operator];
        var acc = e1.elements[0];
        for (var i=1;i < e1.elements.length; i++){
          acc = {
            "type": "BinaryExpression",
            "operator": oper_js,
            "left": acc,
            "right": e1.elements[i]
          }
        }
        return acc;

      case "ListLen":
        return {
          "type": "MemberExpression",
          "computed": false,
          "object": e1,
          "property": {
            "type": "Identifier",
            "name": "length"
          }
        }; 
      case "Head": 
        e2 = {
          "type": "Literal",
          "value": 0,
          "raw": "0"
        };
        return this.memberExpression(e1,e2);
      case "FloatToString": 
        e2 = {
            "type": "Literal",
            "value": "",
            "raw": "\"\""
          };
        return this.binaryExpression(e2,e1);
      //NOpt
      case "ListExpr":  
        var expr_list_js = e1.map((expr) => expr.toJS());
        return {
          "type": "ArrayExpression",
          "elements": expr_list_js
        }
      case "NAry_And": 
      case "NAry_Or": 
        var expr_list_js = e1.map((expr) => expr.toJS());
        var oper_js = nary_opr[this.operator];
        var acc = expr_list_js[0];
        for (var i=1;i < expr_list_js.length; i++){
          acc = {
            "type": "BinaryExpression",
            "operator": oper_js,
            "left": acc,
            "right": expr_list_js[i]
          }
        }
        return acc;

        

      default: throw new Error("Unsupported Argument "+ this.operator)
    }
  }
}

Oper.fromJSON = function(obj){
  return new Oper(obj.value, obj.type);
}


module.exports = Oper;