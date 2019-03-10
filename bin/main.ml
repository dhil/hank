module Settings = struct
  let source_files : string list ref = ref []
end

let cmd_args = []

let usage = "usage: hank <source>"

let _ =
  Arg.parse cmd_args (fun f -> Settings.source_files := f :: !Settings.source_files) usage;
  let () =
    match !Settings.source_files with
    | [] -> Settings.source_files := ["/dev/stdin"]
    | _ -> ()
  in
  List.iter
    (fun source ->
      let open Hank in
      let scanner = Scanner.from_file source in
      try
        let rec consume scanner =
          match Scanner.next scanner with
          | Token.EOF -> [Token.EOF]
          | t -> t :: consume scanner
        in
        let tokens = consume scanner in
        Scanner.close scanner;
        List.iter (fun t -> print_endline (Token.to_string t)) tokens
      with e -> Scanner.close_noerr scanner; raise e)
    !Settings.source_files
