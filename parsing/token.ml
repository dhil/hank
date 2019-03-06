type t =
  | INT of string
  | FLOAT of string
  | CHAR of string
  | STRING of string
  | CASE
  | DATA
  | LET
  | IF
  | IN
  | MODULE
  | THEN
  | WHERE
  | LIDENT of string
  | UIDENT of string
  | BEGIN | END
  | EOF

let to_string = function
  | INT s -> s
  | FLOAT s -> s
  | CHAR s -> s
  | STRING s -> s
  | CASE -> "case"
  | DATA -> "data"
  | LET -> "let"
  | IF  -> "if"
  | IN  -> "in"
  | MODULE -> "module"
  | THEN -> "then"
  | WHERE -> "where"
  | LIDENT s -> s
  | UIDENT s -> s
  | BEGIN -> "begin"
  | END -> "end"
  | EOF -> "EOF"
(* type t =
 *   | BEGIN_BLOCK | END_BLOCK
 *   | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET
 *   | LT | GT
 *   | LET | IN
 *   | DATA
 *   | COMMA | COLON
 *   | RARROW
 *   | BOLDRARROW
 *   | EQUALS
 *   | BAR
 *   | INT of int | CHAR of char | STRING of string
 *   | UIDENT of string
 *   | LIDENT of string
 *   | EOF
 * 
 * let to_string = function
 *   | BEGIN_BLOCK -> "BEGIN"
 *   | END_BLOCK  -> "END"
 *   | LPAREN -> "LPAREN"
 *   | RPAREN -> "RPAREN"
 *   | LBRACE -> "LBRACE"
 *   | RBRACE -> "RBRACE"
 *   | LBRACKET -> "LBRACKET"
 *   | RBRACKET -> "RBRACKET"
 *   | LT -> "LT"
 *   | GT -> "GT"
 *   | LET -> "LET"
 *   | IN -> "IN"
 *   | DATA -> "DATA"
 *   | COMMA -> "COMMA"
 *   | COLON -> "COLON"
 *   | RARROW -> "RARROW"
 *   | BOLDRARROW -> "BOLDRARROW"
 *   | EQUALS -> "EQUALS"
 *   | BAR -> "BAR"
 *   | INT n  -> Printf.sprintf "INT(%d)" n
 *   | CHAR c -> Printf.sprintf "CHAR(%c)" c
 *   | STRING s -> Printf.sprintf "STRING(%s)" s
 *   | UIDENT s  -> Printf.sprintf "UIDENT(%s)" s
 *   | LIDENT s  -> Printf.sprintf "LIDENT(%s)" s
 *   | EOF -> "EOF" *)
