  $ ecma-sl symbolic node_require_a.js
  "B"
  "A"
  - : app = { "type": symbol("normal"), "value": symbol("null"),
              "__completion__": true, "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic node_require_constant_a.js
  "sausage"
  - : app = { "type": symbol("normal"), "value": symbol("null"),
              "__completion__": true, "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic node_require_function_a.js
  "In node_require_function_a.js"
  "chourico"
  - : app = { "type": symbol("normal"), "value": symbol("null"),
              "__completion__": true, "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic node_require_modify_intrinsics_a.js
  "polluted"
  - : app = { "type": symbol("normal"), "value": symbol("null"),
              "__completion__": true, "target": symbol("empty"),  }
  All Ok!
  $ ecma-sl symbolic ./relative-require/a.js
  "a"
  "b"
  "c"
  - : app = { "type": symbol("normal"), "value": symbol("null"),
              "__completion__": true, "target": symbol("empty"),  }
  All Ok!
