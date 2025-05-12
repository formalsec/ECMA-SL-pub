[![Build](https://github.com/formalsec/ECMA-SL/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/formalsec/ECMA-SL/actions/workflows/build.yml)

<h1 align="center", style="font-size: 40px">ECMA-SL Project</h1>

<p align="center">
  <a href="#about">About</a> •
  <a href="#installation">Installation</a> •
  <a href="#ecma-sl-execution">Execution</a> •
  <a href="#integrated-development-environment-ide">IDE</a> •
  <a href="#issues">Issues</a> •
  <a href="#license">License</a>
</p>

<br>

# About

ECMA-SL is a comprehensive platform designed for the specification and execution of the ECMAScript standard, commonly known as JavaScript.
The platform introduces an intermediate language, ECMA-SL, which
serves as a bridge between JavaScript and its execution environment.
This intermediate language is used to develop reference implementations of the ECMAScript standard that adhere to JavaScript's specification.

Key features of the platform include a JavaScript-to-ECMA-SL (JS2ECMA-SL) parser, allowing the conversion of JavaScript code into the ECMA-SL language.
Additionally, ECMA-SL incorporates a compiler from ECMA-SL to Core ECMA-SL, a simplified version of the intermediate language, as well as an interpreter for Core ECMA-SL.
The combination of these tools results in a mechanism to execute JavaScript programs using the reference interpreters for the language.

<br>
<br>





# Installation

The ECMA-SL platform is accessed through the `ecma-sl` application, which is written in the [OCaml] programming language.
To build this application, we employ [dune], a composable build system for OCaml.

1. Install [opam]
2. Bootstrap the OCaml compiler:
<!-- $MDX skip -->
```sh
opam init
opam switch create ecma-sl 4.14.0
eval $(opam env)
```

3. Install the library dependencies:
<!-- $MDX skip -->
```sh
opam update
opam install . --deps-only --with-test
```

4. Build the application and run the available test suit:
<!-- $MDX skip -->
```sh
dune build
dune runtest
```

5. (Optional) Generate code coverage summary/report:
<!-- $MDX skip -->
```sh
BISECT_FILE=`pwd`/bisect dune runtest --force --instrument-with bisect_ppx
bisect-ppx-report summary # Shell summary
bisect-ppx-report html    # Detailed Report in _coverage/index.html
```

6. Install the application on your path:
<!-- $MDX skip -->
```sh
dune install
```

### Integrating OCaml with VSCode

1. Install the [OCaml Platform] extension:
2. Install the OCaml language server and code formatter.
```
opam install ocaml-lsp-server ocamlformat
```

<br>
<br>





# ECMA-SL Reference Execution

The `ecma-sl` application provides the following commands, among others:

- **compile:** to compile an ECMA-SL (`.esl`) program into Core ECMA-SL (`.cesl`).
- **interpret:** to interpret a Core ECMA-SL (`.cesl`) program.
- **encode:** to encode a JavaScript (`.js`) program in Core ECMA-SL (`.cesl`)
- ...

Use `ecma-sl --help` for more information about the entire application, or `ecma-sl <command> --help` for in-depth information regarding a specific command, including a comprehensive list of available options.

<br>

## Compile and Interpret an ECMA-SL Program

- Compile an ECMA-SL `(.esl)` program into Core ECMA-SL (`.cesl`), and interpret the Core ECMA-SL program:
```sh
$ ecma-sl compile test/ecma-sl/test_stdlib_inequality.esl -o distinct.cesl
$ ecma-sl interpret distinct.cesl
```

- Interpret an ECMA-SL `(.esl)` program directly (the application extrapolates the language of the program based on the file extension, and compiles the ECMA-SL program to ECMA-SL if needed):
```sh
$ ecma-sl interpret test/ecma-sl/test_stdlib_inequality.esl
```

### Verbose / Debug Interpretation

- Interpret an ECMA-SL `(.esl)` program in verbose mode (all intermediate interpreter steps are logged):
```sh
$ ecma-sl interpret test/ecma-sl/test_stdlib_inequality.esl --debug full
[ecma-sl] Sucessfuly compiled program 'test/ecma-sl/test_stdlib_inequality.esl'.
[ecma-sl] Sucessfuly evaluated program with return '0'.
```

- Execute an ECMA-SL `(.esl)` program with the debug prompt (breakpoints can be added to `(.esl)` code by preceding the instruction with `#`):
```sh
$ ecma-sl interpret test/ecma-sl/test_stdlib_inequality.esl --db
```

<br>

## Encode and execute a JavaScript Program

- Encode a JavaScript `(.js)` program in Core ECMA-SL `(.cesl)`, and execute the encoded program using the default reference interpreter.
```sh
$ ecma-sl encode test/javascript/simple/catch.js -o catch.cesl
$ ecma-sl execute catch.cesl
Function 'foo' called
Exception caught: 20
Ret: 30
```

- Execute a JavaScript `(.js)` program directly (the application extrapolates the language of the program based on the file extension, and encodes the JavaScript program in Core ECMA-SL if needed):
```sh
$ ecma-sl execute test/javascript/simple/catch.js
Function 'foo' called
Exception caught: 20
Ret: 30
```

- Test a JavaScript `(.js)` program using a test harness:
```sh
$ ecma-sl test --harness=test/javascript/harness.js test/javascript/simple/string.js
---------------------------------------------------------------
 ECMA-SL Test Summary:

test/javascript/simple/string.js ............ SUCCESS ...

---------------------------------------------------------------

Test Summary:


Tests Successful: 1 / 1 (100.00%) | Time elapsed: ...
Failures: 0, Anomalies: 0, Skipped: 0
```

<br>
<br>



# ECMA-SL Symbolic Execution

The `ecma-sl` binary provides a symbolic execution mode via the `symbolic` command.
Symbolic execution requires at least one SMT solver to be installed.

To start using the `symbolic` command, ensure that Z3 is installed by running:

<!-- $MDX skip -->
```sh
opam install z3
```

Afterward, rebuild the `ecma-sl` binary by following the instructions in [installation].

For more information on other available solvers, see [Supported Solvers].


# Integrated Development Environment (IDE)

## Syntax Highlighting for ECMA-SL

- **VSCode -** To start using the VSCode syntax highlighter for ECMA-SL, copy the [extension] it into the `$HOME/.vscode/extensions` folder and restart VSCode.
- **Vim -** To start using the Vim syntax highlighter for ECMA-SL, refer to the [ecmasl-vim] project

<br>
<br>





# Issues

For the list containing all current issues, please consult our [issue-tracker].

<br>
<br>





# License

This project is licensed under the [GPL-3.0 License] - see the [LICENSE] file for details.

[OCaml]: https://ocaml.org/
[dune]: https://github.com/ocaml/dune
[opam]: https://opam.ocaml.org/doc/Install.html
[OCaml Platform]: vscode:extension/ocamllabs.ocaml-platform
[extensions]: extensions/ecmasl-vscode/
[ecmasl-vim]: https://github.com/formalsec/ecmasl-vim
[issue-tracker]: https://github.com/formalsec/ECMA-SL/issues
[GPL-3.0 License]: https://www.gnu.org/licenses/gpl-3.0.en.html
[LICENSE]: ./LICENSE
[installation]: #installation
[Supported Solvers]: https://github.com/formalsec/smtml?tab=readme-ov-file#supported-solvers
