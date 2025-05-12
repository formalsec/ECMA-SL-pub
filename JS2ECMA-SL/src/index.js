#!/usr/bin/env node
const esprima = require("esprima-next");
const yargs = require("yargs");
const fs = require("fs");
const translator = require("./ECMA-SL/translator");
const ParseRegExps = require("./ECMA-SL/utils/parse_regexp");
const { processClasses } = require("./ECMA-SL/utils/processClasses");
const { processTailCalls } = require("./ECMA-SL/utils/processTailCalls");

const argv = yargs
  .option("input", { alias: "i", description: "Input file (.js)", type: "string" })
  .option("output", {
    alias: "o",
    description: "Output file (.esl)",
    type: "string",
  })
  .option("builder", {
    alias: "b",
    description: "Name of the function that builds the AST",
    type: "string",
  })
  .option("compile-to-core", {
    alias: "c",
    description: "Compiles the input directly to Core (.cesl)",
    type: "boolean",
    default: false,
  })
  .option("silent", {
    alias: "s",
    description: "Does not print anything to the console",
    type: "boolean",
    default: false,
  })
  .option("optimised", {
    description: 'Adds the "optimised" property to the AST',
    type: "boolean",
    default: false,
  })
  .demandOption("input")
  .usage("Usage: $0 -i [filepath]")
  .help()
  .alias("help", "h").argv;

fs.readFile(argv.input, "utf-8", (err, data) => {
  if (err) throw err;
  const FUNC_NAME = argv.builder ? argv.builder : "buildAST";

  let prog;
  try {
    progObj = esprima.parseScript(data);
    prog = ParseRegExps(progObj);
    prog = processClasses(prog);
    prog = processTailCalls(prog);
  } catch (ex) {
    prog = newEarlySyntaxError(ex.description);
  }

  if (argv.optimised) {
    prog.optimised = true;
  }

  const statements = translator.fromJSObjectToESLStatements(
    prog,
    argv.compileToCore
  );
  const func = translator.fromESLStatementsToESLFunction(
    FUNC_NAME,
    argv.compileToCore ? ["___internal_esl_global"] : [],
    statements
  );

  if (argv.output) {
    fs.writeFile(argv.output, func.toString(), "utf8", (err) => {
      if (err) throw err;
      if (!argv.silent) console.log("The file has been saved!");
    });
  } else {
    console.log(func.toString());
  }
});

function newEarlySyntaxError(message) {
  return {
    sourceType: "script",
    type: "EarlySyntaxError",
    message: message || ""
  };
}
