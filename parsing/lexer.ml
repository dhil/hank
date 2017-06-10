open Utils
open Continuation
open HParse
open Token

exception LexicalError

module Parser = CharParser(Parser(Singleshot)(struct type t = char end))

let is_lowercase c =
  c >= 'a' && c <= 'z'

let is_uppercase c =
  c >= 'A' && c <= 'Z'

let lowercase =
  Parser.(satisfy is_lowercase)

let uppercase =
  Parser.(satisfy is_uppercase)

let underscore =
  Parser.(char '_')

let lname : (char, token) parser =
  let open Parser in
  let lname = lowercase <*> (many (lowercase <|> uppercase <|> underscore)) in
  (fun (x,xs) -> LNAME (String.implode @@ x :: xs)) <$> lname

let uname =
  let open Parser in
  let lname = uppercase <*> (many (lowercase <|> uppercase <|> underscore)) in
  (fun (x,xs) -> UNAME (String.implode @@ x :: xs)) <$> lname

let integer' : (char, token) parser =
  let open Parser in
  (fun i -> INT (int_of_string i)) <$> integer

let let' =
  let open Parser in
  (fun _ -> LET) <$> string "let"

let in'  =
  let open Parser in
  (fun _ -> IN) <$> string "in"

let symbol s =
  let open Parser in
  (fun s -> SYMBOL s) <$> (string s)

let equal = symbol "="

let lex stream : token Stream.t =
  let open Parser in
  let token_parser =
    let ws = ((many ((peek_char ' ') *> any *> epsilon))) <|> (succeed [()]) in
    (ws *> (many ((lname <|> integer' <|> let' <|> in' <|> equal) <* ws))) <* eos
  in
  let tokens =
    try
      Parser.run stream token_parser
    with
    | SyntaxError -> raise LexicalError
  in
  Stream.of_list tokens
