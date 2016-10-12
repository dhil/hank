{
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n'] (* ignore whitespace and newlines *)
  { token lexbuf }
| '.'
  { DOT }      
| '='
  { EQ }
| ['a'-'z']['A'-'Z']* as name
  { IDENT name }
| "let"
  { LET }
| "in"
  { IN }
| ','
  { COMMA }
| '('
  { LPAR }
| '}'
  { RPAR }
| eof
  { EOF }
| _
  { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
