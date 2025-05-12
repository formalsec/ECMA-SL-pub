# JS2ECMA-SL Tool

A NodeJS tool that encodes a JavaScript program in the memory of ECMA-SL.
This is done by using the [Esprima](https://esprima.org) parser to create an AST in `JSON` with the representation of the `JS` program.



## Installation

1. Install all dependencies:
```sh
npm install
```

## Execute the Parser

1. Encode the JavaScript (`.js`) program into ECMA-SL (`.esl`)

```SH
node src/index.js -i <input.js> -o <output.esl>
```

### Additional Flags
- `-c` to compile directly to Core ECMA-SL (`.cesl`)
- `-b` to change the name of the function that builds the AST
- `--optimized` to add the optimization flag to the compilation