let _ =
  let source =
    "pipe : (<send : A -> ()>B, <recv : A>B) -> B\n\
     pipe (<_>, v) = v\n\
     pipe (<send(msg) -> sender>, <recv -> receiver>) = pipe (sender (), receiver msg)\n\n\
     \
     pipe (<_>, v) = v\n\
     pipe (<send(msg)>, <recv>) => resume = resume((), msg)\n\
     pipe (<send(msg)>, <recv>)           = resume((), msg)"
  in
  List.iter (fun t -> print_endline (Token.to_string t)) (Lexer.tokenise source)
