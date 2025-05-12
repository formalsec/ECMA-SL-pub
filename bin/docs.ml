(* Copyright (C) 2022-2025 formalsec programmers
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

open Cmdliner
open Files.Parser

module ExitCodes = struct
  let ok = Cmdliner.Cmd.Exit.ok

  let compile = 1

  let typing = 2

  let interpret = 3

  let encode = 4

  let execute = 5

  let test = 6

  let sym_abort = 20

  let sym_assert_failure = 21

  let sym_eval_failure = 22

  let sym_exec_failure = 23

  let sym_readFile_failure = 24

  let sym_failure = 25

  let generic = 40

  let term = 122

  let client = Cmdliner.Cmd.Exit.cli_error

  let internal = Cmdliner.Cmd.Exit.internal_error
end

module Exits = struct
  open Cmdliner.Cmd.Exit

  let app = info ~doc:"on term error" ExitCodes.term :: defaults

  let common = info ~doc:"on generic client-side error" ExitCodes.generic :: app

  let compile = info ~doc:"on ECMA-SL compiling error" ExitCodes.compile

  let typing = info ~doc:"on ECMA-SL typing error" ExitCodes.typing

  let interpret = info ~doc:"on ECMA-SL runtime error" ExitCodes.internal

  let encoding = info ~doc:"on JavaScript encoding error" ExitCodes.encode

  let execute = info ~doc:"on JavaScript execution error" ExitCodes.execute

  let test = info ~doc:"on JavaScript test error" ExitCodes.test

  let symbolic =
    [ info ~doc:"on symbolic execution abort" ExitCodes.sym_abort
    ; info ~doc:"on symbolic assertion failure" ExitCodes.sym_assert_failure
    ; info ~doc:"on reaching symbolic eval sink" ExitCodes.sym_eval_failure
    ; info ~doc:"on reaching symbolic exec sink" ExitCodes.sym_exec_failure
    ; info ~doc:"on reaching symbolic readFile sink"
        ExitCodes.sym_readFile_failure
    ; info ~doc:"on unexpected symbolic execution failure" ExitCodes.sym_failure
    ]
end

module Copyright = struct
  let s_copyright = "COPYRIGHT"

  let disclamer =
    "ecma-sl Copyright (C) 2022-2025  formalsec\n\
     This program comes with ABSOLUTELY NO WARRANTY; for details type: \
     `ecma-sl --help`.\n\
     This is free software, and you are welcome to redistribute it\n\
     under certain conditions."
end

module CommonOpts = struct
  let debug =
    let docs = Manpage.s_common_options in
    let docv = "LEVEL" in
    let doc =
      "Debug level used within the ECMA-SL application. Options include: (1) \
       'none' for hiding all ECMA-SL logs; (2) 'warn' [default] for showing \
       ECMA-SL warnings; and (3) 'full' to show all, including debug prints."
    in
    let levels = Arg.enum Enums.DebugLvl.(args all) in
    Arg.(value & opt levels Warn & info [ "debug" ] ~docs ~docv ~doc)

  let colorless =
    let docs = Manpage.s_common_options in
    let doc =
      "Generate colorless output. This flag might be necessary for terminals \
       lacking 16-ANSI-color support."
    in
    Arg.(value & flag & info [ "colorless" ] ~docs ~doc)
end

module FileOpts = struct
  let input =
    let docv = "FILE" in
    let doc = "Name of the input file." in
    Arg.(required & pos 0 (some non_dir_fpath) None & info [] ~docv ~doc)

  let inputs =
    let docv = "FILE/DIR" in
    let doc = "Name of the input files or input directories." in
    Arg.(non_empty & pos_all valid_fpath [] & info [] ~docv ~doc)

  let output =
    let docv = "FILE" in
    let doc = "Name of the output file." in
    Arg.(value & opt (some fpath) None & info [ "o"; "output" ] ~docv ~doc)
end

module CompileOpts = struct
  let untyped =
    let doc =
      "Run the ECMA-SL compiler without performing static type checking. In \
       this mode, all type annotations are ignored."
    in
    Arg.(value & flag & info [ "untyped" ] ~doc)
end

module CompileCmd = struct
  let sdocs = Manpage.s_common_options

  let doc = "Compiles an ECMA-SL program to Core ECMA-SL"

  let description =
    [| "Given an ECMA-SL (.esl) file, compiles the program to the Core ECMA-SL \
        (.cesl) language."
    |]

  let man = [ `S Manpage.s_description; `P (Array.get description 0) ]

  let man_xrefs = []

  let exits = Exits.compile :: Exits.typing :: Exits.common
end

module InterpretOpts = struct
  let lang =
    let docv = "LANG" in
    let doc =
      "Language of the program to be interpreted. Options include: (1) 'auto' \
       [default] for inferring the language based on the file extension; (2) \
       'esl' for ECMA-SL (.esl) files; (3) 'cesl' for Core ECMA-SL (.cesl) \
       files; and (4) 'cesl-unattached' for executing Core ECMA-SL (.cesl) \
       without certain restrictions imposed by the ECMA-SL compiler, such as \
       the predefined return value format: (success, value)."
    in
    let langs = Arg.enum Enums.Lang.(args Cmd_interpret.Options.langs) in
    Arg.(value & opt langs Auto & info [ "lang" ] ~docv ~doc)

  let tracer =
    let doc =
      "Show the interpretation steps, including the evaluation of statements \
       and expressions. Options include: (1) 'none' [default] for no trace \
       information; (2) 'call' for tracing function calls and return values; \
       (3) 'step' for tracing ECMA-SL statements; (4) 'full' for tracing \
       ECMA-SL statements and expression evaluations; and (5) 'core' for \
       tracing Core ECMA-SL intermediate steps."
    in
    let tracers = Arg.enum Enums.InterpTracer.(args @@ all ()) in
    Arg.(value & opt tracers None & info [ "trace" ] ~doc)

  let tracer_loc =
    let doc =
      "Show the locations of every language construct under evaluation. This \
       option is only used when the trace mode is not set to 'none'."
    in
    Arg.(value & flag & info [ "trace-loc" ] ~doc)

  let tracer_depth =
    let doc =
      "Maximum call stack depth logged by the tracer. Non-positive depth \
       values are ignored by the command. Depth value 0 [default] represents \
       unlimited trace depth. Note that this option is only used when the \
       trace mode is not set to 'none'."
    in
    Arg.(value & opt int 0 & info [ "trace-depth" ] ~doc)

  let debugger =
    let doc =
      "Enable the ECMA-SL debugger. The debug prompt will start after \
       encountering the first breakpoint. Breakpoints can be inserted in \
       ECMA-SL (.esl) or Core ECMA-SL (.cesl) code by preceding any statement \
       with the '#' character."
    in
    Arg.(value & flag & info [ "db"; "debugger" ] ~doc)

  let profiler =
    let doc =
      "Show profiling information, including execution times, memory usage, \
       and the number of expressions/statements evaluated. Options include (1) \
       'none' [default] for no profilling information; (2) 'time' for \
       execution time information only; and (3) 'full' for complete profilling \
       information."
    in
    let profilers = Arg.enum Enums.InterpProfiler.(args @@ all ()) in
    Arg.(value & opt profilers None & info [ "profiler" ] ~doc)

  let main =
    let docv = "FUNC" in
    let doc =
      "Designated entry point function for the interpreter. Caution: modifying \
       this function can lead to unforeseen outcomes during interpretation, as \
       certain constraints enforced by the ECMA-SL compiler may be affected \
       (e.g., accesses to global variables)."
    in
    Arg.(value & opt string "main" & info [ "main" ] ~docv ~doc)

  let print_depth =
    let doc =
      "Maximum print depth of the Core ECMA-SL (.cesl) interpreter when \
       recursively printing objects values. Negative depth values are ignored \
       by the command. Depth value 0 prints all objects as {...}. Not \
       specifying a depth [default] results in unlimited object print depth."
    in
    Arg.(value & opt (some int) None & info [ "print-depth" ] ~doc)

  let show_exitval =
    let doc =
      "Display the value returned by the top-level function, typically the \
       'main' function, at the end of the program interpretation."
    in
    Arg.(value & flag & info [ "exitval" ] ~doc)
end

module InterpretCmd = struct
  let sdocs = Manpage.s_common_options

  let doc = "Interprets a Core ECMA-SL program"

  let description =
    [| "Given an ECMA-SL (.esl) or Core ECMA-SL (.cesl) file, executes the \
        program using the concrete interpreter for Core ECMA-SL. When provided \
        with an ECMA-SL (.esl) file, defaults to compiling the program into \
        Core ECMA-SL (.cesl) before execution, while keeping important \
        metadata regarding the original ECMA-SL source code."
     ; "Some of the options from the 'compile' command are also available when \
        interpreting an ECMA-SL (.esl) file. When running the interpreter \
        directly on a Core ECMA-SL file, these options are ignored."
    |]

  let man =
    [ `S Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ]

  let man_xrefs = [ `Page ("ecma-sl compile", 1) ]

  let exits = Exits.compile :: Exits.typing :: Exits.interpret :: Exits.common
end

module EncodeOpts = struct
  let builder =
    let docv = "FUNC" in
    let doc =
      "Name of the function responsible for reconstructing the Abstract Syntax \
       Tree (AST) of the JavaScript program in the ECMA-SL memory. By default, \
       this function is called 'buildAST'"
    in
    Arg.(value & opt (some string) None & info [ "builder" ] ~docv ~doc)
end

module EncodeCmd = struct
  let sdocs = Manpage.s_common_options

  let doc = "Encodes a JavaScript program in Core ECMA-SL"

  let description =
    [| "Given a JavaScript (.js) file, encodes the program in the Core ECMA-SL \
        (.cesl) language. This is done through a two-stage process. First, \
        generate the Abstract Syntax Tree (AST) of the JavaScript program \
        using Esprima, an official JavaScript parser. Then, translate the \
        resulting AST into the Core ECMA-SL language (.cesl), introducing \
        minor adjustments to meet the expectations of ECMARef interpreters."
    |]

  let man = [ `S Manpage.s_description; `P (Array.get description 0) ]

  let man_xrefs = []

  let exits = Exits.encoding :: Exits.common
end

module ExecuteOpts = struct
  let lang =
    let docv = "LANG" in
    let doc =
      "Language of the program to be executed. Options include: (1) 'auto' \
       [default] for inferring the language based on the file extension; (2) \
       'js' for JavaScript (.js) files; and (3) 'cesl' for Core ECMA-SL \
       (.cesl) files."
    in
    let langs = Arg.enum Enums.Lang.(args Cmd_execute.Options.langs) in
    Arg.(value & opt langs Auto & info [ "lang" ] ~docv ~doc)

  let jsinterp =
    let docv = "INTERP" in
    let doc =
      "Version of the reference interpreter. Options include: (1) 'main' \
       [default] for the most complete interpreter; (2) 'latest' for the most \
       recent interpreter; (3) 'ecmaref5' for the ES5 reference interpreter; \
       (4) 'ecmaref6' for the ES6 reference interpreter; and (5) \
       'ecmaref6-sym' for the ES6 symbolic interpreter."
    in
    let interps = Arg.enum Enums.JSInterp.(args @@ all ()) in
    Arg.(value & opt interps Main & info [ "interp" ] ~docv ~doc)

  let harness =
    let docv = "FILE" in
    let doc = "Name of the JavaScript (.js) harness file." in
    Arg.(value & opt (some non_dir_fpath) None & info [ "harness" ] ~docv ~doc)
end

module ExecuteCmd = struct
  let sdocs = Manpage.s_common_options

  let doc = "Executes an encoded JavaScript program"

  let description =
    [| "Given an JavaScript program encoded in Core ECMA-SL (.cesl), executes \
        the program using a JavaScript interpreter. When provided with an \
        unencoded JavaScript (.js) program, defaults to encoding the program \
        into Core ECMA-SL (.cesl) before execution."
     ; "The JavaScript reference interpreter (ECMARef interpreters) are \
        written in ECMA-SL, and adhere to the JavaScript standard \
        line-by-line. The option '--interp' specifies the interpreter that \
        will be used to execute the program. Besides the ES5 and ES6 reference \
        interpreters, there are other interpreters available such as the \
        symbolic ES6 interpreter. Additionaly, the '--harness' flag can be \
        used to specify a JavaScript program that will be executed before the \
        main program."
     ; "Some of the options of the 'interpret' command are also available. \
        These include the ability to enable the ECMA-SL and debugger, show the \
        final result of the program, among others."
    |]

  let man =
    [ `S Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ; `P (Array.get description 2)
    ]

  let man_xrefs =
    [ `Page ("ecma-sl encode", 1); `Page ("ecma-sl interpret", 2) ]

  let exits =
    Exits.compile
    :: Exits.typing
    :: Exits.interpret
    :: Exits.encoding
    :: Exits.execute
    :: Exits.common
end

module TestOpts = struct
  let test_type =
    let doc =
      "The type of the test that will be executed. Options include: (1) 'auto' \
       [default] that automatically identifies the test kind based on the its \
       contents; (2) 'simple' for simple JavaScript tests that never throw \
       errors; and (3) 'test262' tests from the official JavaScript \
       conformance testsuit (https://github.com/tc39/test262). Note that the \
       'auto' test kind may not always yield the correct result."
    in
    let test_types = Arg.enum Enums.JSTest.(args @@ all ()) in
    Arg.(value & opt test_types Auto & info [ "type" ] ~doc)

  let report =
    let docv = "FILE/DIR" in
    let doc = "Name of the file or directory for storing the test report." in
    Arg.(value & opt (some fpath) None & info [ "report" ] ~docv ~doc)

  let webhook_url =
    let doc =
      "Specify a webhook url on which to send a summary of the test results. \
       By default this is disabled."
    in
    Arg.(value & opt (some string) None & info [ "webhook-url" ] ~doc)

  let jobs =
    let doc = "Specify the number of jobs to run. " in
    Arg.(value & opt int 1 & info [ "jobs" ] ~doc)
end

module TestCmd = struct
  let sdocs = Manpage.s_common_options

  let doc = "Executes an encoded JavaScript test"

  let description =
    [| "Given a JavaScript test file encoded in Core ECMA-SL (.cesl), executes \
        the test using the JavaScript interpreter. When provided with an \
        unencoded JavaScript (.js) test, defaults to encoding the test into \
        Core ECMA-SL (.cesl) before execution."
     ; "Running a JavaScript test is similar to executing a regular JavaScript \
        program with the following distinctions. All JavaScript logs (prints) \
        are redirected, and the test's final return value is analyzed to \
        determine if the test ran successfully or not. As a result, most of \
        the options from the 'execute' command are also available."
     ; "Additionally, it is possible to run tests with custom formats, \
        according to the options of the '--kind' option. This formats include \
        the 'test262' format for tests from the official JavaScript \
        conformance testsuit. Detailed test results can be redirected to a \
        directory with the '--report' option."
    |]

  let man =
    [ `S Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ; `P (Array.get description 2)
    ]

  let man_xrefs = [ `Page ("ecma-sl execute", 1) ]

  let exits =
    Exits.compile
    :: Exits.typing
    :: Exits.interpret
    :: Exits.encoding
    :: Exits.execute
    :: Exits.test
    :: Exits.common
end

module SymbolicOpts = struct
  let lang =
    let docv = "LANG" in
    let doc =
      "Language of the program to be executed. Options include: (1) 'auto' \
       [default] for inferring the language based on the file extension; (2) \
       'js' for JavaScript (.js) files; (3) 'esl' for ECMA-SL (.esl) files; \
       and (3) 'cesl' for Core ECMA-SL (.cesl) files."
    in
    let langs = Arg.enum Enums.Lang.(args Cmd_symbolic.valid_languages) in
    Arg.(value & opt langs Auto & info [ "lang" ] ~docv ~doc)

  let target =
    let docv = "FUNC" in
    let doc = "Designated entry point for the analysis." in
    Arg.(value & opt string "main" & info [ "target"; "t" ] ~docv ~doc)

  let workspace =
    let docv = "DIR" in
    let doc = "The workspace directory for the results of the analysis." in
    let default = Fpath.v "ecma-out" in
    Arg.(value & opt fpath default & info [ "workspace"; "w" ] ~docv ~doc)
end

module SymbolicCmd = struct
  let sdocs = Manpage.s_common_options

  let doc = "Performs symbolic analysis on an ECMA-SL program"

  let description =
    [| "Given an JavaScript program encoded in Core ECMA-SL (.cesl), runs the \
        program using the ECMA-SL symbolic engine. When provided with an \
        unencoded JavaScript (.js) program, defaults to encoding the program \
        into Core ECMA-SL (.cesl) before execution. Similarly, when provided \
        with an ECMA-SL program (.esl), defaults to compiling the program."
     ; "The JavaScript symbolic interpreter (ecmaref-sym interpreters) are \
        written in ECMA-SL, and extend the concrete interpreters with symbolic \
        calls. The option '--target' specifies the entry point of the symbolic \
        analysis. Additionally, the '--workspace' option can be used to set \
        the directory where the result of the analysis will be stored."
    |]

  let man =
    [ `S Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ]

  let man_xrefs =
    [ `Page ("ecma-sl compile", 1)
    ; `Page ("ecma-sl encode", 2)
    ; `Page ("ecma-sl execute", 3)
    ]

  let exits =
    Exits.compile
    :: Exits.typing
    :: Exits.interpret
    :: Exits.encoding
    :: Exits.symbolic
    @ Exits.common
end

module Application = struct
  let sdocs = Manpage.s_common_options

  let doc = "Executable specification of the ECMAScript standard"

  let version = "%%VERSION%%"

  let description =
    [| "ECMA-SL is a comprehensive platform designed for the specification and \
        execution of the ECMAScript standard, commonly known as JavaScript. \
        The platform introduces an intermediate language, ECMA-SL, which \
        serves as a bridge between JavaScript and its execution environment. \
        This intermediate language is used to provide a reference \
        implementation of the ECMAScript standard that adheres to JavaScript's \
        specification."
     ; "Key features of the platform include a JavaScript-to-ECMA-SL \
        (JS2ECMA-SL) parser, allowing the conversion of JavaScript code into \
        the ECMA-SL language. Additionally, ECMA-SL incorporates a compiler \
        from ECMA-SL to Core ECMA-SL, a simplified version of the platform's \
        language, as well as an interpreter for Core ECMA-SL. By combining \
        these tools, one can execute a JavaScript program using the reference \
        interpreters for JavaScript."
     ; "Use ecma-sl <command> --help for more information on a specific \
        command."
    |]

  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P (Array.get description 0)
    ; `P (Array.get description 1)
    ; `P (Array.get description 2)
    ; `S Manpage.s_common_options
    ; `P "These options are common to all commands."
    ; `S Manpage.s_bugs
    ; `P "Check bug reports at https://github.com/formalsec/ECMA-SL/issues."
    ; `S Copyright.s_copyright
    ; `P Copyright.disclamer
    ]

  let man_xrefs = []

  let exits = Exits.app
end
