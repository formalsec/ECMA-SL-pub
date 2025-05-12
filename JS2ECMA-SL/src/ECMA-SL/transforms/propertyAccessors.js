module.exports = {
  /**
   * Transform a Esprima MemberStatement that is not computed in a computed one.
   * One that is not computed represents a property accessor expression using the dot notation form:
   *    MemberExpression . IdentifierName
   *    CallExpression . IdentifierName
   *
   * One that is computed represents a property accessor expression using the bracket notation:
   *    MemberExpression [ Expression ]
   *    CallExpression [ Expression ]
   *
   * The dot notation is explained by the following syntactic conversion:
   *    MemberExpression . IdentifierName
   *
   * is identical in its behaviour to:
   *    MemberExpression [ <identifier-name-string> ]
   *
   * and similarly
   *    CallExpression . IdentifierName
   *
   * is identical in its behaviour to
   *    CallExpression [ <identifier-name-string> ]
   *
   * where <identifier-name-string> is a string literal containing the same sequence of characters after processing
   * of Unicode escape sequences as the IdentifierName.
   *
   *
   * Since the standard only defines one production for both types of property accessors:
   *    Production MemberExpression : MemberExpression [ Expression ]
   *
   * We transform a not computed one (that is in dot notation) in a computed one (that is in bracket notation).
   */
  transform: function (obj) {
    if (obj.computed === false) {
      return {
        ...obj,
        type: obj.type,
        object: obj.object,
        property: {
          type: "Literal",
          value: obj.property.name,
          raw: encloseInDoubleQuotes(obj.property.name),
        },
        computed: true,
      };
    }
    
    return obj;
  },
};

function encloseInDoubleQuotes(value) {
  if (value.startsWith('"')) {
    return `"${value.split('"').join('\\"')}"`;
  }
  return `"${value}"`;
}
