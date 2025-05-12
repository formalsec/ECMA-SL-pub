const mapper = require("./mapper");

module.exports = {
  processClasses: ProcessClasses,
};



/*

state:
  nomes de funcoes -> construtores

*/

function ProcessClasses (obj) {
  function callback (obj) {
    switch (obj.type) {
      case "ClassDeclaration": {
        addConstructor(obj)
        return {
          obj,
          recurse: true
        }
      }
      case "ClassExpression": {
        addConstructor(obj)
        return {
          obj,
          recurse: true
        }
      }
      case "MethodDefinition": {
        return {
          obj,
          recurse: true
        }
      }
      case "CallExpression": {
        if (obj.callee.type === "Super") {
          var new_obj = {
            type: "SuperCall",
            arguments: obj.arguments
          }
          return {
            obj: new_obj,
            recurse: true
          }
        } else {
          return {
            obj,
            recurse: true
          }
        }
      }
      case "MemberExpression": {
        if (obj.object.type == "Super") {
          return {
            obj: {
              type: "SuperProperty",
              property: obj.property,
            },
            recurse: true
          }
        } else {
          return {
            obj,
            recurse: true
          }
        }
      }
       default:
        return {
          obj,
          recurse: true
        }
    }
  }

  return mapper(callback, obj)
}


function noConstructor(ast) {
  return !ast.body.body.some(el => {
    if (el.type === "MethodDefinition") {
      return el.key.name === "constructor"
    }
    return false
  })
}

function hasSuper(ast) {
  return !!ast.superClass
}

function createConstructorWithSuper() {
  return  {
    "type": "MethodDefinition",
    "key": {
      "type": "Identifier",
      "name": "constructor"
    },
    "computed": false,
    "value": {
      "type": "FunctionExpression",
      "id": null,
      "params": [{
        "type": "RestElement",
        "argument": {
          "type": "Identifier",
          "name": "args"
        }
      }],
      "body": {
        "type": "BlockStatement",
        "body": [
          {
            "type": "ExpressionStatement",
            "expression": {
              "type": "SuperCall",
              "arguments": [
                {
                  "type": "SpreadElement",
                  "argument": {
                    "type": "Identifier",
                    "name": "args"
                  }
                }
              ],
            },
          }
        ]
      },
      "generator": false,
      "expression": false,
      "async": false
    },
    "kind": "constructor",
    "static": false
  }
}

function createConstructor() {
  return  {
    "type": "MethodDefinition",
    "key": {
      "type": "Identifier",
      "name": "constructor"
    },
    "computed": false,
    "value": {
      "type": "FunctionExpression",
      "id": null,
      "params": [],
      "body": {
        "type": "BlockStatement",
        "body": []
      },
      "generator": false,
      "expression": false,
      "async": false
    },
    "kind": "constructor",
    "static": false
  }
}

function getConstructorParams(ast) {
  if (noConstructor(ast)) {
    return []
  }
  return ast.body.body.find(el => {
    return el.type === "MethodDefinition" && el.key.name === "constructor"
  })
}

function addConstructor(obj) {
  if (noConstructor(obj) && hasSuper(obj)) {
    obj.body.body.push(createConstructorWithSuper())
  } else if (noConstructor(obj)) {
    obj.body.body.push(createConstructor())
  }
}
