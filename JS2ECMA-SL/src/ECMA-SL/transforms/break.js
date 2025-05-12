module.exports = {
  transform: function (obj) {
    if (obj.type !== "BreakStatement") {
      throw Error('Unexpected object type; Expecting "BreakStatement"');
    }

    if (obj.label !== null && typeof obj.label === "object") {
      obj.label = obj.label.name;
    }

    return obj;
  },
};
