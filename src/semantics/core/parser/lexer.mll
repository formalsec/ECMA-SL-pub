(* ========== OCaml Utilities ========== *)

{
  open Lexing
  open Parser

  let keywords = Hashtbl.of_seq @@ List.to_seq
          [
            (* Language values *)
            "null"                    , NULL;

            (* Language constructs *)
            "function"                , FUNCTION;
            "print"                   , PRINT;
            "return"                  , RETURN;
            "delete"                  , DELETE;
            "extern"                  , EXTERN;
            "assert"                  , ASSERT;
            "fail"                    , FAIL;
            "if"                      , IF;
            "else"                    , ELSE;
            "while"                   , WHILE;
            "switch"                  , SWITCH;
            "case"                    , CASE;
            "default"                 , DEFAULT;

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
      | true  -> Printf.sprintf "%s. Line number: %d." msg (lexbuf.lex_curr_p.pos_lnum)
      | false -> Printf.sprintf "%s: %s. Line number: %d." msg c (lexbuf.lex_curr_p.pos_lnum)
    ) in (Syntax_error formatted_msg)
}



(* ========== Regular expressions ========== *)

let digit         = ['0' - '9']
let letter        = ['a' - 'z' 'A' - 'Z']
let int           = '-'? digit+
let frac          = '.' digit*
let exp           = ['e' 'E'] ['-' '+']? digit+
let float         = digit+ frac? exp? | frac exp? | "nan" | "inf"
let bool          = "true" | "false"
let id            = (letter | '_') (letter | digit | '_')* '\''*
let gid           = '|' (id) '|'
let symbol        = '\'' (id | int)
let white         = (' ' | '\t')+
let newline       = '\r' | '\n' | "\r\n"
let three_d       = digit digit digit
let char_code     = '\\' three_d



(* ========== Lexical rules ========== *)

rule read =
  parse
  | white             { read lexbuf }
  | newline           { new_line lexbuf; read lexbuf }
  | ','               { COMMA }
  | ';'               { SEMICOLON }
  | ':'               { COLON }
  | ":="              { DEFEQ }
  | "@"               { ATSIGN }
  | "#"               { HASH }
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
  | "=="              { EQ }
  | "!="              { NE }
  | '<'               { LT }
  | '>'               { GT }
  | "<="              { LE }
  | ">="              { GE }
  | '"'               { create_string lexbuf (read_string (Buffer.create 16)) }
  | int               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool              { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | id as x           { try Hashtbl.find keywords x with Not_found -> ID x }
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
  | '\\' (three_d as c)   {
                            Buffer.add_char buf (Char.chr (int_of_string c));
                            read_string buf lexbuf
                          }
  | char_code char_code   {
                            let s = Lexing.lexeme lexbuf in
                            let s' = "\"" ^ s ^ "\"" in
                            let s'' = Scanf.sscanf s' "%S" (fun s -> s) in
                            Buffer.add_string buf s'';
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
