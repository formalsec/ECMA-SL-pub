const {
  getVarDeclarations,
  getFunctionDeclarations,
  replaceFuncDeclarations,
  getLetDeclarations,
  getConstDeclarations
} = require("../utils/getDeclarations");
const { hasStrictDirective } = require("../utils/strict");

module.exports = {
  transform: function (obj) {
    if (obj.type !== "Program") {
      throw Error('Unexpected object type; Expecting "Program"');
    }

    const variableDeclarations = getVarDeclarations(obj).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );

    const letDeclarations = getLetDeclarations(obj).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );

    const constDeclarations = getConstDeclarations(obj).reduce(
      // remove repeated variables
      (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
      []
    );

    const functionDeclarations = getFunctionDeclarations(obj);

    obj.body = obj.body.map(replaceFuncDeclarations);
    obj.variableDeclarations = variableDeclarations;
    obj.functionDeclarations = functionDeclarations;
    
    obj.letDeclarations = letDeclarations;
    obj.constDeclarations = constDeclarations;
    obj.codeType = "global";

    obj.strict = hasStrictDirective(obj.body);
    return obj;
  },
};
