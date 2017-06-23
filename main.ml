open Utils
open HParse
open Syntax

(* let () = *)
(*   let source = "42 foo {} [ ] | bar" in *)
(*   try *)
(*     let file = open_in "testsuite/paper.fk" in *)
(*     let lexbuf = Lexing.from_channel file in *)
(*     let eos = ref false in *)
(*     let token_stream = *)
(*       Stream.from *)
(*         (fun _ -> *)
(*           match Lexer.read lexbuf with *)
(*           | tok when Loc.Located.item tok = EOF -> *)
(*              if !eos *)
(*              then None *)
(*              else (eos := true; Some tok) *)
(*           | tok -> Some tok) *)
(*     in *)
(*     Stream.iter (fun t -> Printf.printf "%s " (string_of_token (Loc.Located.item t))) token_stream; *)
(*     close_in file *)
(*   with Lexer.SyntaxError err -> Printf.fprintf stderr "Syntax error: %s\n" err *)
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


module Tok = struct
  open Loc
  type t = Token.t Located.t
  let to_string t = Token.to_string @@ Located.item t
  let compare t1 t2 =
    let _ =
      Printf.printf "Comparing %s and %s\n" (to_string t1) (to_string t2);
    in
    Token.compare (Located.item t1) (Located.item t2)

  let lift : Token.t -> t
    = fun t -> Loc.Located.lift_dummy t

  let unpack : t -> Token.t
    = fun t -> Loc.Located.item t
end

let grammar (module P : PARSER with type token = Tok.t) =
  let open P in
  let open Token in
  let lift = Tok.lift in
  let brackets p =
    enclosed ~left:(lift LBRACKET) ~right:(lift RBRACKET) p
  in
  let parens p =
    enclosed ~left:(lift LPAREN) ~right:(lift RPAREN) p
  in
  let identifier =
    let ident =
      satisfy
        (fun t -> match Loc.Located.item t with
        | NAME _ -> true
        | _ -> false)
    in
    let transform tok =
      match Tok.unpack tok with
      | NAME name -> Name name
      | _ -> assert false
    in
    transform <$> ident
  in
  let operator s =
    token (lift (OPERATOR s))
  in
  let effect_var =
    let parse () =
      match eval @@ optional identifier with
      | None -> Name "£"
      | Some ident -> ident
    in
    of_comp parse
  in
  let tyvars =
    let effvar = (brackets effect_var) in
    let tyvar  = identifier in
    effvar <|> tyvar
  in
  let type' =
    let parenthesised= parens identifier in
    let basic = identifier in
    parenthesised <|> basic
  in
  let constr : Syntax.constr P.t =
    let tag = identifier in
    let params = many type' in
    let transform (Name name, params) =
      Constr (name, [])
    in
    transform <$> (tag <*> params)
  in
  let variant_declaration t0 =
    let data   = token t0 in
    let tyvars = many tyvars in
    let equal = operator "=" in
    let constrs = separated_list ~sep:(lift (OPERATOR "|")) constr in
    let parse () =
      let _ = eval data in
      let name = eval identifier in
      let tyvars = eval tyvars in
      let _    = eval equal in
      let constrs = eval constrs in
      ()
    in
    of_comp parse
  in
  let data_declaration =
    variant_declaration (lift DATA)
  in
  let eof = token (lift EOF) in
  data_declaration <* eof

let () =
  if not !Sys.interactive then
    let module Parser = Parser(Continuation.Singleshot)(Tok) in
    try
      (*let file = open_in "testsuite/paper.fk" in*)
      let source = "data Foo a = bar Int | baz Char String" in
      (*let lexbuf = Lexing.from_channel file in*)
      let lexbuf = Lexing.from_string source in
      let eos = ref false in
      let token_stream =
      Stream.from
        (fun _ ->
          match Lexer.read lexbuf with
          | tok when Loc.Located.item tok = Token.EOF ->
             if !eos
             then None
             else (eos := true; Some tok)
          | tok -> Some tok)
      in
      let (s1, s2) = Stream.tee token_stream in
      Stream.iter (fun t -> Printf.printf "%s " (Token.to_string (Loc.Located.item t))) s1;
      Printf.printf "\n"; flush stdout;
      (*close_in file;*)
      Parser.run s2 (grammar (module Parser))
    with Lexer.SyntaxError err -> Printf.fprintf stderr "Syntax error: %s\n" err
