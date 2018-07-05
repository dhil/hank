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

let keywords = [
  "data", DATA;
  "let", LET;
  "in", IN
]

}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name = ['A'-'Z' 'a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let op_initial = ['!' '<' '>' '?' '$' ':' ';' '|' '&' '+' '-' '*' '/' '^' '~' '=']
let operator = op_initial (op_initial | '!')*

let comment = "--" [^ '\r' '\n']*

rule read =
  parse
  | white    { read lexbuf }
  | comment  { read lexbuf }
  | "{-"     { read_multiline_comment lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | name     {  let s = lexeme lexbuf in
                try List.assoc s keywords with
                | Not_found ->
                   if Char.code s.[0] >= 65 && Char.code s.[0] <= 90 then UIDENT s
                   else LIDENT s
             }
  | int      {  (INT (int_of_string (lexeme lexbuf)))  }
  | '{'      {  LBRACE  }
  | '}'      {  RBRACE  }
  | '('      {  LPAREN  }
  | ')'      {  RPAREN  }
  | '['      {  LBRACKET  }
  | ']'      {  RBRACKET  }
  | "=>"     {  BOLDRARROW }
  | "->"     {  RARROW }
  | '<'      {  LT  }
  | '>'      {  GT  }
  | '='      {  EQUALS }
  | ','      {  COMMA  }
  | '"'      {  (read_string (Buffer.create 17) lexbuf)  }
  | "'"      {  (CHAR (read_char lexbuf))  }
  | ':'      {  COLON  }
  | eof      {  EOF  }
  | _ { raise (SyntaxError ("Unexpected char: " ^ lexeme lexbuf)) }

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
    { Buffer.add_string buf (lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { raise (SyntaxError ("String is not terminated")) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ lexeme lexbuf)) }

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
  | [^ '\'' '\\'] { lexeme_char lexbuf 0 }
  | eof       { raise (SyntaxError ("Character is not terminated")) }
  | "'"       { raise (SyntaxError ("Empty character")) }
  | _         { raise (SyntaxError ("Illegal character: " ^ lexeme lexbuf)) }

and read_multiline_comment =
  parse
  | "-}"      { read lexbuf }
  | eof       { raise (SyntaxError ("Comment is not terminated")) }
  | _         { read_multiline_comment lexbuf }

{
let tokenise s =
  let rec next buf =
     match read buf with
     | EOF -> []
     | t -> t :: next buf
  in
  next (Lexing.from_string s)
}