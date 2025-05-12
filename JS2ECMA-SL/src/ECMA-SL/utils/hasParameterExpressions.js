module.exports = {
  hasParameterExpression: hasParameterExpression
};

function hasParameterExpression(params) {
  return params.some(param => paramHasParameterExpression(param));
}

function paramHasParameterExpression(param) {
  if (!param) return
  switch(param.type) {
    case "Identifier":
      return false
    case "RestElement":
      return getParamName(param.argument);
    case "Property":
      return getParamName(param.value);
    case "ObjectPattern":
      return param.properties.some(getParamName);
    case "ArrayPattern":
      return param.elements.some(getParamName);
    case "AssignmentPattern":
      return true
  }
}
