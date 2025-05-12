# Unreleased Versions

### Changed
### Added

- Improves symbolic execution prints
- Adds `s_join` to ecma-sl's standard library
- Adds `fpath`, a simple library to manipulate paths, to ecma-sl's standard library
- Adds `os`, a simple library to manipulate files, to ecma-sl's standard library
- Implements commonjs require statement

### Fixed

# v1.1.0

### Added

- Line comments to ECMA-SL and Core ECMA-SL language.
- The `skip` flag for Test262 tests to allow tests to be skipped.

### Changed

- The ECMA-SL library exposes the share site through the module `Ecma_sl.Share`.
- Symbolic expressions were removed from the language.
- Math/string/list operators were removed from the syntax and became external functions.
- The Explode-js commands and share files were removed.
- The `sdefault` keyword was replaced by the `default` keyword.
    - This keyword is now shared by both the match-with and switch statements.
- The `elif` keyword was removed from the `.esl` syntax.
    - It can now be fully replaced by the `else if` construct.
- The semicolon `;` usage was changed in the `.esl` syntax:
    - Imports/typedefs/simple statements terminate a semicolon;
    - Functions/compound statements don't terminate with a semicolon;
    - A lonely semicolon results in the `skip` statement.
- Bitwise operators are now defined over integers instead of floats.
- The values used by ECMA-SL (`Val.ml`) were changed to the values of `Smtml`. In the next code snippet, you can see how it was converted:
<!-- $MDX skip -->
```ocaml
type t =
  | Void                      (* -> App "void" [] *)
  | Null                      (* -> App "null" [] *)
  | Int of int                (* -> Int *)
  | Flt of (float[@unboxed])  (* -> Real *)
  | Str of string             (* -> Str *)
  | Bool of bool              (* -> True | False *)
  | Symbol of string          (* -> App "symbol" [Str s] *)
  | Loc of Loc.t              (* -> App "loc" [Int l] *)
  | Byte of int               (* Int *)
  | List of t list            (* -> List l *)
  | Arr of t array            (* removed from language *)
  | Tuple of t list           (* removed from language *)
  | Type of Type.t            (* removed from language *)
  | Curry of string * t list  (* -> App fn fvs *)
```
- Operators `exp`, `random`, and tuple related operators were removed from syntax and became external functions.

### Fixed

- The conditional operator now only evaluates one of the branches in the concrete interpreter.
- Source regions for string values are now properly calculated.

# v1.0.0

### Added

- Starts changelog in `CHANGES.md` (@filipeom)
