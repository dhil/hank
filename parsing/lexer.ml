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

let is_letter c =
  is_lowercase c || is_uppercase c

let is_digit c =
  c >= '0' && c <= '9'

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

let data =
  let open Parser in
  (fun _ -> DATA) <$> string "data"

let interface =
  let open Parser in
  (fun _ -> INTERFACE) <$> string "interface"

let symbol s =
  let open Parser in
  (fun s -> SYMBOL s) <$> (string s)

let equal = symbol "="
let parens p = Parser.enclosed '(' p ')'
let braces p = Parser.enclosed '{' p '}'
let brackets p = Parser.enclosed '[' p ']'
let chevrons p = Parser.enclosed '<' p '>'
let arrow = symbol "->"
let bar = symbol "|"

let comma = Parser.char ','

let spaces' p = Parser.(p <* spaces)

let separated_list p sep =
  let open Parser in
  let cons = fun (x,xs) -> x :: xs in
  let elems = p <*> (many (sep *> p)) in
  (cons <$> elems) <|> (succeed [])


let pos t =
  let open Parser in
  (fun x -> (x,())) <$> t

let lex stream : (token * unit) Stream.t =
  let open Parser in
  let name = pos (lname <|> uname) in
  let token_parser =
    (*let ws = ((many ((satisfy (fun c -> c = ' ')) *> any *> epsilon))) <|> (succeed [()]) in*)
    (*    (ws *> (many ((lname <|> integer' <|> let' <|> in' <|> equal) <* spaces))) <* eos*)
    (spaces *> (separated_list (spaces' name) (spaces' comma))) <* eos
  in
  let tokens : ( (token * unit) list ) =
    try
      Parser.run stream token_parser
    with
    | SyntaxError -> raise LexicalError
  in
  Stream.of_list tokens
