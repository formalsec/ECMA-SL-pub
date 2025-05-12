module.exports = {
  getParamsNames: getParamsNames,
  getParamName: getParamName
};

function getParamsNames(params) {
  let paramsNames = params.map(getParamName)
  const flattenArray = (arr) => arr.reduce((a, b) => a.concat(Array.isArray(b) ? flattenArray(b) : b), []);
  const flattenedArray = flattenArray(paramsNames).filter(param => param);
  return flattenedArray
}

function getParamName(param) {
  if (!param) return
  switch(param.type) {
    case "Identifier":
      return param.name;
    case "RestElement":
      return getParamName(param.argument);
    case "Property":
      return getParamName(param.value);
    case "ObjectPattern":
      return param.properties.map(getParamName);
    case "ArrayPattern":
      return param.elements.map(getParamName);
    case "AssignmentPattern":
      return getParamName(param.left);
  }
}
