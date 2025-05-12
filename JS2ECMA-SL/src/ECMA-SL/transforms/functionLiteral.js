const {
  getLetDeclarations,
  getConstDeclarations,
  getVarDeclarations,
  getFunctionDeclarations,
  replaceFuncDeclarations,
} = require("../utils/getDeclarations");
const { isSimpleParameterList } = require("../utils/isSimpleParameterList");
const { containsExpressions } = require("../utils/containsExpression");
const { hasStrictDirective } = require("../utils/strict");
const { getParamsNames } = require("../utils/getParamsNames");

module.exports = {
  transform: function (obj) {
    if (!["FunctionExpression", "FunctionDeclaration", "ArrowFunctionExpression"].includes(obj.type)) {
      throw Error(
        'Unexpected object type; Expecting "FunctionExpression", "FunctionDeclaration" or "ArrowFunctionExpression"'
      );
    }


    const paramsDetails = obj.params;
    const paramsNames = getParamsNames(paramsDetails);

    const variableDeclarations = getVarDeclarations(obj.body).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );

    const letDeclarations = getLetDeclarations(obj.body).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );

    const constDeclarations = getConstDeclarations(obj.body).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );


    const functionDeclarations = getFunctionDeclarations(obj.body);

    obj.body = replaceFuncDeclarations(obj.body);

    obj.body.isSimpleParameterList = isSimpleParameterList(obj.params);
    obj.body.variableDeclarations = variableDeclarations;
    obj.body.functionDeclarations = functionDeclarations;
    obj.body.letDeclarations = letDeclarations;
    obj.body.constDeclarations = constDeclarations;
    obj.body.containsExpression = containsExpressions(obj.params);
    obj.body.paramsDetails = paramsDetails;
    obj.body.paramsNames = paramsNames;

    obj.params = paramsNames;

    obj.body.codeType = "function";

    obj.body.strict = hasStrictDirective(obj.body.body);

    return obj;
  },
};
