module.exports = {
  containsExpressions: paramsContainsExpression,
};

function paramsContainsExpression(params) {
  const hasExpression = params.some(param => paramHasParameterExpression(param))
  return hasExpression
}

function paramHasParameterExpression(param) {
  if (!param) return
  switch(param.type) {
    case "Identifier":
      return false
    case "RestElement":
      return paramHasParameterExpression(param.argument);
    case "Property":
      return paramHasParameterExpression(param.value);
    case "ObjectPattern":
      return param.properties.some(paramHasParameterExpression);
    case "ArrayPattern":
      return param.elements.some(paramHasParameterExpression);
    case "AssignmentPattern":
      return true
  }
}
