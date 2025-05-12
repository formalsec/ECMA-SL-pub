Test basic symbolic number:
  $ ecma-sl symbolic basic_symbolic_number.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic basic_symbolic_string.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_arith.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_number_branching.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_operations.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_branching.js
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  - : app = { "type": symbol("normal"), "value": 0, "__completion__": true,
              "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic symbolic_string_array.js
  "":83159.2-83159.20: Assert failure:
   Stmt: assert (hd params)
   Expr: false
  Path condition: (bool.eq "banana bread" (str.++ (flour, " ", water)))
  Model:
   (model
     (flour str "banana")
     (water str "bread"))
  Found 1 problems!
  [21]
