module.exports = {
  transform: function (obj) {
    if (obj.type !== "CallExpression") {
      throw Error('Unexpected object type; Expecting "CallExpression"');
    }

    if (obj.callee.name === "ESLPrint") {
      return {
        type: "ESLPrint",
        value: obj.arguments[0]
      };
    }
    return obj;
  },
};
