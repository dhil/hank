{
open Lexing
open Token

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let located token lexbuf =
  let start = lexeme_start_p lexbuf in
  let end'  = lexeme_end_p lexbuf in
  Loc.Located.lift token start end'
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name = ['A'-'Z' 'a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let comment = "--" [^ '\r' '\n']*

rule read =
  parse
  | white    { read lexbuf }
  | comment  { read lexbuf }
  | "{-"     { read_multiline_comment lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "let"    { located LET lexbuf }
  | "in"     { located IN lexbuf }
  | "interface" { located INTERFACE lexbuf }
  | "data"   { located DATA lexbuf }
  | name     { located (NAME (Lexing.lexeme lexbuf)) lexbuf }
  | int      { located (INT (int_of_string (Lexing.lexeme lexbuf))) lexbuf }
  | '='      { located EQUAL lexbuf }
  | '{'      { located LBRACE lexbuf }
  | '}'      { located RBRACE lexbuf }
  | '('      { located LPAREN lexbuf }
  | ')'      { located RPAREN lexbuf }
  | '['      { located LBRACKET lexbuf }
  | ']'      { located RBRACKET lexbuf }
  | '<'      { located LT lexbuf }
  | '>'      { located GT lexbuf }
  | ','      { located COMMA lexbuf }
  | '"'      { located (read_string (Buffer.create 17) lexbuf) lexbuf }
  | "'"      { located (CHAR (read_char lexbuf)) lexbuf }
  | "->"     { located ARROW lexbuf }
  | ':'      { located COLON lexbuf }
  | ';'      { located SEMICOLON lexbuf }
  | '|'      { located BAR lexbuf }
  | '!'      { located BANG lexbuf }
  | eof      { located EOF lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { raise (SyntaxError ("String is not terminated")) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_char =
  parse
  | '\\' '/'  { '/' }
  | '\\' '\\' { '\\' }
  | '\\' 'b'  { '\b' }
  | '\\' 'f'  { '\012' }
  | '\\' 'n'  { '\n' }
  | '\\' 'r'  { '\r' }
  | '\\' 't'  { '\t' }
  | '\\' "'"  { '\'' }
  | [^ '\'' '\\'] { Lexing.lexeme_char lexbuf 0 }
  | eof       { raise (SyntaxError ("Character is not terminated")) }
  | "'"       { raise (SyntaxError ("Empty character")) }
  | _         { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_multiline_comment =
  parse
  | "-}"      { read lexbuf }
  | eof       { raise (SyntaxError ("Comment is not terminated")) }
  | _         { read_multiline_comment lexbuf }
