const regexpTree = require("regexp-tree");
const mapper = require("./mapper");
const addParenIndexAndParenCount = require("../../regexp/utils");

function countGroups(re) {
  var count = 0;
  regexpTree.traverse(re, {
    Group({node}) {
      if (node.capturing) {
        count +=1;
      }
    }
  });
  return count;
}

function parseRegExps(obj) {
  function callback(obj) {
    if (!obj)
      return {
        obj,
        recurse: false,
      };

    switch (obj.type) {
      case "Literal":
        if (obj.hasOwnProperty("regex")) {

          let re = regexpTree.parse(obj.raw);
          re.nCaps = countGroups(re);
          addParenIndexAndParenCount(re);

          return {
            obj: {
              type: "Literal",
              value: obj.raw,
              raw: obj.raw,
              regex: re,
            },
            recurse: false,
          };
        }

        return {
          obj,
          recurse: false,
        };

      default:
        return {
          obj,
          recurse: true,
        };
    }
  }

  return mapper(callback, obj);
}

module.exports = parseRegExps;
