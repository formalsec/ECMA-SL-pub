const getParamsNames = require("../utils/getParamsNames").getParamsNames;

module.exports = {
  transform: function (obj) {
    if (obj.type !== "ForInStatement"  && obj.type !== "ForOfStatement") {
      throw Error('Unexpected object type; Expecting "ForInStatement"');
    }

    if (obj.left !== null && typeof obj.left === "object") {
      if (obj.left.type === "VariableDeclaration" ) {
        obj.left.declarations[0].kind = obj.left.kind;
        obj.left = obj.left.declarations[0];
      }
      if (obj.left.kind == "let" || obj.left.kind == "const"){
        obj.left.boundNames = getParamsNames([obj.left.id]);
        obj.left.isConst = obj.left.kind === "const";
      }
    }

    return obj;
  },
};
