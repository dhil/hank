module Settings = struct
  let source_files : string list ref = ref []
end

let cmd_args = []

let usage = "usage: hank <source>"

let _ =
  Arg.parse cmd_args (fun f -> Settings.source_files := f :: !Settings.source_files) usage;
  match !Settings.source_files with
  | [] -> print_endline usage
  | _ ->
     List.iter
       (fun source ->
         let ic = open_in source in
         try
           let lexbuf = Lexing.from_channel ic in
           let tokens = Hank.Lexer.tokenise lexbuf in
           close_in ic;
           List.iter (fun t -> print_endline (Hank.Token.to_string t)) tokens
         with e -> close_in_noerr ic; raise e)
       !Settings.source_files
