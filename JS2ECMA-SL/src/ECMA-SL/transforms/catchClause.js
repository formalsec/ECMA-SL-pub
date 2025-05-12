const { getParamsNames } = require("../utils/getParamsNames");

module.exports = {
  transform: function (obj) {
    
    if (obj.type !== "CatchClause") {
        throw Error('Unexpected object type; Expecting "CatchClause"');
    }

    const paramDetails = obj.param;

    const paramsNames = getParamsNames([paramDetails]);
    obj.paramsNames = paramsNames;
    return obj;
  },
};