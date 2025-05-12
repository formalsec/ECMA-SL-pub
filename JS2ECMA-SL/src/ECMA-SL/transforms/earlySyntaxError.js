module.exports = {
  transform: function (obj) {
    if (obj.type !== "EarlySyntaxError") {
      throw Error('Unexpected object type; Expecting "EarlySyntaxError"');
    }

    obj.body = [];
    obj.variableDeclarations = [];
    obj.functionDeclarations = [];

    return obj
  }
}
