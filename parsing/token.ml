type token =
  | NAME of string
  | INT of int
  | LET
  | IN
  | DATA
  | INTERFACE
  | STRING of string
  | CHAR of char
  | OPERATOR of string
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | ARROW
  | EQUAL
  | LT
  | GT
  | BAR
  | COLON
  | SEMICOLON
  | COMMA
  | BANG
  | EOF

let string_of_token =
  let open Printf in
  function
  | NAME s -> sprintf "NAME(%s)" s
  | INT i -> sprintf "INTEGER(%d)" i
  | LET -> "LET"
  | IN  -> "IN"
  | DATA -> "DATA"
  | INTERFACE -> "INTERFACE"
  | STRING s -> sprintf "STRING(%s)" s
  | CHAR c -> sprintf "CHAR(%c)" c
  | OPERATOR s -> sprintf "OPERATOR(%s)" s
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | ARROW -> "ARROW"
  | EQUAL -> "EQUAL"
  | LT -> "LT"
  | GT -> "GT"
  | BAR -> "BAR"
  | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON"
  | COMMA -> "COMMA"
  | BANG -> "BANG"
  | EOF -> "EOF"
