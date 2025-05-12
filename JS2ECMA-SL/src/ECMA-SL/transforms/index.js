const Program = require("./program");
const Switch = require("./switch");
const PropertyAccessors = require("./propertyAccessors");
const Assignment = require("./assignment");
const FunctionLiteral = require("./functionLiteral");
const FunctionCall = require("./functionCall");
const Literal = require("./literal");
const EarlySyntaxError = require("./earlySyntaxError");
const ForIn = require("./forIn");
const Break = require("./break");
const Continue = require("./continue");
const CatchClause = require("./catchClause");
const Block = require("./block");
const For = require("./for");

module.exports = {
  transformObject: function (obj) {
    if (obj.type === "Program") {
      return Program.transform(obj);
    }
    if (obj.type === "EarlySyntaxError") {
      return EarlySyntaxError.transform(obj);
    }
    if (obj.type === "ForInStatement" || obj.type === "ForOfStatement") {
      return ForIn.transform(obj);
    }
    if (obj.type === "ContinueStatement") {
      return Continue.transform(obj);
    }
    if (obj.type === "BreakStatement") {
      return Break.transform(obj);
    }
    if (obj.type === "SwitchStatement") {
      return Switch.transform(obj);
    }
    if (obj.type === "MemberExpression") {
      return PropertyAccessors.transform(obj);
    }
    if (obj.type === "AssignmentExpression") {
      return Assignment.transform(obj);
    }
    if (
      obj.type === "FunctionExpression" ||
      obj.type === "FunctionDeclaration" ||
      obj.type === "ArrowFunctionExpression"
    ) {
      return FunctionLiteral.transform(obj);
    }
    if (obj.type === "CallExpression") {
      return FunctionCall.transform(obj);
    }
    if (obj.type === "Literal") {
      return Literal.transform(obj);
    }
    if (obj.type === "CatchClause") {
      return CatchClause.transform(obj);
    }
    if (obj.type === "BlockStatement") {
      return Block.transform(obj);
    }
    if (obj.type === "ForStatement") {
      return For.transform(obj);
    }
    return obj;
  },
};
