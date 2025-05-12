const getLetDeclarations = require("../utils/getDeclarations").getLetDeclarations;
const getConstDeclarations = require("../utils/getDeclarations").getConstDeclarations;

module.exports = {
    transform: function (obj) {
      if (obj.type !== "ForStatement") {
            throw Error('Unexpected object type; Expecting "ForStatement"');
      }

      const letDeclarations = getLetDeclarations(obj.init).reduce(
        // remove repeated variables
        (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
        []
      );
      if (letDeclarations.length > 0){
        obj.lexicalDeclarations = letDeclarations;
        obj.isConstant = false;
        return obj;
      }
      
      const constDeclarations = getConstDeclarations(obj.init).reduce(
        // remove repeated variables
        (acc, localVar) => (acc.includes(localVar) ? acc : acc.concat(localVar)),
        []
      );

      if (constDeclarations.length > 0){
        obj.lexicalDeclarations = constDeclarations;
        obj.isConstant = true;
      }
      
      return obj;
    },
};