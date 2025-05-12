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

open EslBase
open EslSyntax
include Parsing_utils

type 'a estart = Lexing.position -> 'a EParser.MenhirInterpreter.checkpoint

type etoken = [%import: EParser.token] [@@deriving show]

let elexer (last_token : etoken ref) (lexbuf : Lexing.lexbuf) =
  let token = ELexer.read lexbuf in
  last_token := token;
  token

let eparser (start : 'a estart) (lexbuf : Lexing.lexbuf) : EProg.t =
  let module ESLMI = EParser.MenhirInterpreter in
  let last_token = ref EParser.EOF in
  ESLMI.loop_handle
    (fun result -> result)
    (function
      | ESLMI.Rejected -> Log.fail "Parser rejected input"
      | ESLMI.HandlingError _e ->
        Log.stderr "%a, last token: %s: %s.@." print_position lexbuf
          (show_etoken !last_token) "Error message found";
        raise EParser.Error
      | _ -> Log.fail "Unexpected state in failure handler!" )
    (ESLMI.lexer_lexbuf_to_supplier (elexer last_token) lexbuf)
    (start lexbuf.Lexing.lex_curr_p)

let parse_eexpr ?(file : string = "") (str : string) : EExpr.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_expr_target ELexer.read lexbuf

let parse_estmt ?(file : string = "") (str : string) : EStmt.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_stmt_target ELexer.read lexbuf

let parse_efunc ?(file : string = "") (str : string) : EFunc.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_func_target ELexer.read lexbuf

let parse_etype ?(file : string = "") (str : string) : EType.t =
  let lexbuf = init_lexbuf file str in
  EParser.entry_type_target ELexer.read lexbuf

let parse_eprog ?(file : string = "") (path : string) (str : string) : EProg.t =
  let lexbuf = init_lexbuf file str in
  let prog = eparser EParser.Incremental.entry_prog_target lexbuf in
  { prog with file; path }
