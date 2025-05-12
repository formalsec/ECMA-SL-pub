module.exports = {
  transform: function (obj) {
    if (obj.type !== "ContinueStatement") {
      throw Error('Unexpected object type; Expecting "ContinueStatement"');
    }

    if (obj.label !== null && typeof obj.label === "object") {
      obj.label = obj.label.name;
    }

    return obj;
  },
};
