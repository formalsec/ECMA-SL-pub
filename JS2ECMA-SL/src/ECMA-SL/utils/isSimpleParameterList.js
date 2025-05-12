module.exports = {
  isSimpleParameterList: isSimpleParameterList,
};


function isSimpleParameterList(params) {
  types = params.map((param) => param.type);
  return params.every((param) => param.type == "Identifier")
};
