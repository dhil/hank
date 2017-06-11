open Utils
(*open HParse*)
open Token

let () =
  let source = "42 foo {} [ ] | bar" in
  try
    let file = open_in "testsuite/paper.fk" in
    let lexbuf = Lexing.from_channel file in
    let eos = ref false in
    let token_stream =
      Stream.from
        (fun _ ->
          match Lexer.read lexbuf with
          | tok when Loc.Located.item tok = EOF ->
             if !eos
             then None
             else (eos := true; Some tok)
          | tok -> Some tok)
    in
    Stream.iter (fun t -> Printf.printf "%s " (string_of_token (Loc.Located.item t))) token_stream;
    close_in file
  with Lexer.SyntaxError err -> Printf.fprintf stderr "Syntax error: %s\n" err
  (* let stream = Stream.of_string source in *)
  (* try *)
  (*   let tokens : (token * unit) Stream.t = lex stream in *)
  (*   Stream.iter (fun (t,()) -> Printf.printf "%s " (string_of_token t)) tokens *)
  (* with *)
  (* | LexicalError -> Printf.printf "Lexical error!\n" *)
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
