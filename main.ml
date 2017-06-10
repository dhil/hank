open Utils
open HParse
open Lexer
open Token

let () =
  let source = "  foobar   42abba" in
  let stream = Stream.of_string source in
  try
    let tokens = lex stream in
    Stream.iter (fun t -> Printf.printf "%s " (string_of_token t)) tokens
  with
  | LexicalError -> Printf.printf "Lexical error!\n"
(*  let module MyParser = Parser(Continuation.Singleshot)(struct type t = char end) in
  let module CP = CharParser(MyParser) in
  let parse s =
    let g = CP.(natural) in
    CP.run s g
  in
  match parse stream with
  | exception SyntaxError -> Printf.printf "Syntax error!\n"
  | res -> Printf.printf "%s\n" res*)
  (*let lineBuffer = Lexing.from_channel stdin in
  try
    Parser.prog Lexer.read lineBuffer
  with
  | Lexer.SyntaxError msg -> Printf.fprintf stderr "%s%!\n" msg
  | Parser.Error -> Printf.fprintf stderr "Syntax error!\n"*)
