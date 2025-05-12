module.exports = {
  transform: function(obj) {
    if (obj.type !== "Identifier") {
      throw Error('Unexpected object type; Expecting "Identifier"');
    }

    if (obj.name === "undefined") {
      return {
        type: "Literal",
        value: "'undefined",
        raw: "\"'undefined\""
      }
    }

    return obj
  }
}
