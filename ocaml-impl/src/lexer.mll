{
open Parser

exception Lexical_error of string

let keyword_table = Hashtbl.create 53
let () =
  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "program", PROGRAM;
      "var", VAR;
      "val", VAL;
      "type", TYPE;
      "def", DEF;
      "if", IF;
      "else", ELSE;
      "while", WHILE;
      "for", FOR;
      "to", TO;
      "of", OF;
      "array", ARRAY;
      "record", RECORD;
      "end", END;  (* Still needed for record end *)
      "integer", TINTEGER;
      "real", TREAL;
      "boolean", TBOOLEAN;
      "char", TCHAR;
      "string", TSTRING;
      "true", TRUE;
      "false", FALSE;
      "and", AND;
      "or", OR;
      "not", NOT;
      "div", DIV;
      "mod", MOD;
      "writeln", WRITELN;
      "write", WRITE;
      "readln", READLN;
      "new", NEW;
      "return", RETURN;
    ]
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*
let integer = digit+
let real = digit+ '.' digit+ (['e' 'E'] ['+' '-']? digit+)?
let whitespace = [' ' '\t' '\r']+
let newline = '\n'

rule token = parse
  | whitespace    { token lexbuf }
  | newline       { Lexing.new_line lexbuf; token lexbuf }
  | "//"          { line_comment lexbuf }
  | "(*"          { block_comment lexbuf }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '['           { LBRACK }
  | ']'           { RBRACK }
  | '{'           { LBRACE }
  | '}'           { RBRACE }
  | '.'           { DOT }
  | ','           { COMMA }
  | ':'           { COLON }
  | ';'           { SEMICOLON }
  | ":="          { ASSIGN }
  | '='           { EQ }
  | "<>"          { NE }
  | '<'           { LT }
  | "<="          { LE }
  | '>'           { GT }
  | ">="          { GE }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { STAR }
  | '/'           { SLASH }
  | '^'           { CARET }
  | '@'           { AT }
  | ".."          { DOTDOT }
  | integer as i  { INTEGER (int_of_string i) }
  | real as f     { REAL (float_of_string f) }
  | '\'' ([^ '\''] as c) '\'' { CHAR c }
  | '\'' '\'' '\''            { CHAR '\'' }
  | '"' ([^ '"']* as s) '"'   { STRING s }
  | ident as id   {
      match Hashtbl.find_opt keyword_table (String.lowercase_ascii id) with
      | Some tok -> tok
      | None -> IDENT id
    }
  | eof           { EOF }
  | _ as c        { raise (Lexical_error (Printf.sprintf "Unexpected character: %c" c)) }

and line_comment = parse
  | newline       { Lexing.new_line lexbuf; token lexbuf }
  | eof           { EOF }
  | _             { line_comment lexbuf }

and block_comment = parse
  | "*)"          { token lexbuf }
  | newline       { Lexing.new_line lexbuf; block_comment lexbuf }
  | eof           { raise (Lexical_error "Unclosed comment") }
  | _             { block_comment lexbuf }
