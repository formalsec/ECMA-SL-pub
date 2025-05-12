module.exports = {
  /**
   * This is an experimental feature.
   * Used for the purposes of testing the assignment of the "Prototype" property that every object contains.
   * That's why the "obj.left.property.name" has a so specific value.
   */
  transform: function (obj) {
    if (obj.type !== "AssignmentExpression") {
      throw Error('Unexpected object type; Expecting "AssignmentExpression"');
    }
    if (
      obj.left.type === "MemberExpression" &&
      obj.left.property.name === "__ecmasl_proto__"
    ) {
      return {
        type: "ProtoAssignment",
        object: obj.left.object,
        value: obj.right,
      };
    }

    return obj;
  },
};
