const getLetDeclarations = require("../utils/getDeclarations").getLetDeclarations;
const getConstDeclarations = require("../utils/getDeclarations").getConstDeclarations;

module.exports = {
  /**
   * Transform a Esprima SwitchStatement in a slightly different object that better fits the needs of
   * interpreting the specification of the switch statement as defined in the ES5 standard.
   */
  transform: function (obj) {
    const newObj = {
      type: obj.type,
      discriminant: obj.discriminant,
      cases: [],
    };

    const casesA = [];
    const casesB = [];
    let defaultCase = null;
    obj.cases.forEach((caze) => {
      if (defaultCase === null && caze.test) {
        casesA.push(caze);
      } else if (caze.test === null) {
        defaultCase = caze;
      } else {
        casesB.push(caze);
      }
    });

    newObj.cases.push(casesA);
    newObj.cases.push(defaultCase);
    newObj.cases.push(casesB);
    newObj.letDeclarations = getLetDeclarations(obj)
    newObj.constDeclarations = getConstDeclarations(obj)

    return newObj;
  },
};
