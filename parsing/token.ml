type token =
  | LNAME of string
  | UNAME of string
  | INT of int
  | LET
  | IN
  | SYMBOL of string
  | DATA
  | INTERFACE

let string_of_token =
  let open Printf in
  function
  | LNAME s -> sprintf "LNAME(%s)" s
  | UNAME s -> sprintf "UNAME(%s)" s
  | INT i -> sprintf "INT(%d)" i
  | LET -> "LET"
  | IN -> "IN"
  | SYMBOL s -> sprintf "SYMBOL(%s)" s
  | DATA -> "DATA"
  | INTERFACE -> "INTERFACE"
