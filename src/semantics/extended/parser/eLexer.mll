(* ========== OCaml Utilities ========== *)

{
  open Lexing
  open EParser

  let keywords = Hashtbl.of_seq @@ List.to_seq
          [
            (* Language values *)
            "null"                    , NULL;
            "None"                    , NONE;

            (* Language constructs *)
            "let"                     , LET;
            "import"                  , IMPORT;
            "typedef"                 , TYPEDEF;
            "macro"                   , MACRO;
            "function"                , FUNCTION;
            "print"                   , PRINT;
            "return"                  , RETURN;
            "delete"                  , DELETE;
            "extern"                  , EXTERN;
            "lambda"                  , LAMBDA;
            "assert"                  , ASSERT;
            "fail"                    , FAIL;
            "throw"                   , THROW;
            "catch"                   , CATCH;
            "if"                      , IF;
            "else"                    , ELSE;
            "while"                   , WHILE;
            "foreach"                 , FOREACH;
            "repeat"                  , REPEAT;
            "until"                   , UNTIL;
            "switch"                  , SWITCH;
            "case"                    , CASE;
            "default"                 , DEFAULT;
            "match"                   , MATCH;
            "with"                    , WITH;

            (* List operators *)
            "hd"                      , LIST_HEAD;
            "tl"                      , LIST_TAIL;

            (* Type operators *)
            "typeof"                  , TYPEOF;
            "int_to_float"            , INT_TO_FLOAT;
            "int_to_string"           , INT_TO_STRING;
            "float_to_int"            , FLOAT_TO_INT;
            "float_to_string"         , FLOAT_TO_STRING;
            "string_to_int"           , STRING_TO_INT;
            "string_to_float"         , STRING_TO_FLOAT;

            (* Object operators *)
            "obj_to_list"             , OBJECT_TO_LIST;
            "obj_fields"              , OBJECT_FIELDS;
            "in_obj"                  , OBJECT_MEM;

            (* Type system *)
            "any"                     , TYPE_ANY;
            "unknown"                 , TYPE_UNKNOWN;
            "never"                   , TYPE_NEVER;
            "undefined"               , TYPE_UNDEFINED;
            "void"                    , TYPE_VOID;
            "int"                     , TYPE_INT;
            "float"                   , TYPE_FLOAT;
            "string"                  , TYPE_STRING;
            "boolean"                 , TYPE_BOOLEAN;
            "symbol"                  , TYPE_SYMBOL;
            "sigma"                   , TYPE_SIGMA;

            (* FIXME: (Math constants) Should go to the stdlib? *)
            "NaN"                     , FLOAT (Float.nan);
            "Infinity"                , FLOAT (Float.infinity);
            "MaxValue"                , FLOAT (Float.max_float);
            "MinValue"                , FLOAT (5e-324);
            "Pi"                      , FLOAT (Float.pi);
          ]

  exception Syntax_error of string

  let create_string (lexbuf : Lexing.lexbuf) (read_string : Lexing.lexbuf -> token): token =
    let start_p = lexbuf.lex_start_p in
    let token = read_string lexbuf in
    lexbuf.lex_start_p <- start_p;
    token

  let create_syntax_error ?(eof=false) (msg : string) (lexbuf : Lexing.lexbuf) : exn =
    let c = Lexing.lexeme lexbuf in
    let formatted_msg = (
      match eof with
      | true  -> Printf.sprintf "%s. Line number: %d. File: %s" msg (lexbuf.lex_curr_p.pos_lnum) (lexbuf.lex_curr_p.pos_fname)
      | false -> Printf.sprintf "%s: %s. Line number: %d. File: %s" msg c (lexbuf.lex_curr_p.pos_lnum) (lexbuf.lex_curr_p.pos_fname)
    ) in (Syntax_error formatted_msg)
}



(* ========== Regular expressions ========== *)

let digit         = ['0' - '9']
let letter        = ['a' - 'z' 'A' - 'Z']
let int           = '-'? digit+
let frac          = '.' digit*
let exp           = ['e' 'E'] ['-' '+']? digit+
let float         = digit+ frac? exp? | frac exp?
let bool          = "true" | "false"
let id            = (letter | '_') (letter | digit | '_')* '\''*
let gid           = '|' (id) '|'
let symbol        = '\'' (id | int)
let white         = (' ' | '\t')+
let newline       = '\r' | '\n' | "\r\n"
let hex_digit     = digit | ['A'-'F' 'a'-'f']
let hex_literal   = "0x" hex_digit+



(* ========== Lexical rules ========== *)

rule read =
  parse
  | white             { read lexbuf }
  | newline           { new_line lexbuf; read lexbuf }
  | ','               { COMMA }
  | ';'               { SEMICOLON }
  | ':'               { COLON }
  | '.'               { PERIOD }
  | "="               { DEFEQ0 }
  | ":="              { DEFEQ }
  | '@'               { ATSIGN }
  | '#'               { HASH }
  | "->"              { ARROW }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '{'               { LBRACE }
  | '}'               { RBRACE }
  | '['               { LBRACK }
  | ']'               { RBRACK }
  | '?'               { QUESTION }
  | '!'               { EXCLAMATION }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIVIDE }
  | '%'               { MODULO }
  | "**"              { POW }
  | '~'               { TILDE }
  | '&'               { AMPERSAND }
  | '|'               { PIPE }
  | '^'               { CARET }
  | "<<"              { SHIFT_LEFT }
  | ">>"              { SHIFT_RIGHT }
  | ">>>"             { SHIFT_RIGHT_LOGICAL }
  | "&&"              { LAND }
  | "||"		          { LOR }
  | "&&&"             { SCLAND }
  | "|||"             { SCLOR }
  | "=="              { EQ }
  | "!="              { NE }
  | '<'               { LT }
  | '>'               { GT }
  | "<="              { LE }
  | ">="              { GE }
  | '"'               { create_string lexbuf (read_string (Buffer.create 16)) }
  | int               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | hex_literal       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool              { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | id as x           { try Hashtbl.find keywords x with Not_found -> ID x }
  | gid               { GID (String_utils.trim_ends (Lexing.lexeme lexbuf))}
  | symbol            { SYMBOL (String_utils.chop_first_char (Lexing.lexeme lexbuf)) }
  | "//"              { read_line_comment lexbuf }
  | "/*"              { read_block_comment lexbuf }
  | _                 { raise (create_syntax_error "Unexpected char" lexbuf) }
  | eof               { EOF }

(* ========== String reader ========== *)

and read_string buf =
  parse
  | '"'                   { STRING (Buffer.contents buf)                         }
  | '\\' '\\'             { Buffer.add_char buf '\\';    read_string buf lexbuf  }
  | '\\' 'b'              { Buffer.add_char buf '\b';    read_string buf lexbuf  }
  | '\\' 'n'              { Buffer.add_char buf '\n';    read_string buf lexbuf  }
  | '\\' 'r'              { Buffer.add_char buf '\r';    read_string buf lexbuf  }
  | '\\' 't'              { Buffer.add_char buf '\t';    read_string buf lexbuf  }
  | '\\' '0'              { Buffer.add_char buf '\000';  read_string buf lexbuf  }
  | '\\' 'v'              { Buffer.add_char buf '\011';  read_string buf lexbuf  }
  | '\\' 'f'              { Buffer.add_char buf '\012';  read_string buf lexbuf  }
  | '\\' '\"'             { Buffer.add_char buf '\"';    read_string buf lexbuf  }
  | '\\' 'x' hex_digit hex_digit as h
                          {
                            Buffer.add_string buf (String_utils.hexdecode h);
                            read_string buf lexbuf
                          }
  | '\\' 'u' '{' hex_digit hex_digit hex_digit hex_digit hex_digit? hex_digit? '}' as h
                          {
                            Buffer.add_string buf (String_utils.utf8decode h);
                            read_string buf lexbuf
                          }
  | [^ '"' '\\']+         {
                            Buffer.add_string buf (Lexing.lexeme lexbuf);
                            read_string buf lexbuf
                          }
  | _                     { raise (create_syntax_error "Illegal string character" lexbuf) }
  | eof                   { raise (create_syntax_error ~eof:true "String is not terminated" lexbuf) }

(* ========== Comment reader ========== *)

and read_line_comment =
  parse
  | newline   { new_line lexbuf; read lexbuf }
  | _         { read_line_comment lexbuf }

and read_block_comment =
  parse
  | "*/"      { read lexbuf }
  | newline   { new_line lexbuf; read_block_comment lexbuf }
  | _         { read_block_comment lexbuf }
  | eof       { raise (create_syntax_error ~eof:true "Comment is not terminated" lexbuf)}
