type cmd_args =
  { verbose: bool
  ; version: bool
  ; sources: string list
  ; frank_mode: bool
  ; flex_only: bool
  }
    
let handle_args verbose version sources frank_mode flex_only =
  if version then
    (Printf.printf "%s\n" Version.version; exit 0)
  else if List.length sources = 0 && not version then
    `Error (true,  "no sources given")
  else
    `Ok (Printf.printf "verbose = %b\nversion = %b\nsrcs = %s\n" verbose version (String.concat ", " sources);
         { verbose = verbose
         ; version = version
         ; sources = sources
         ; frank_mode = frank_mode
         ; flex_only = flex_only }
        )

(* Command line interface *)
open Cmdliner

(* Standard options *)
let verbose =
  let doc = "Use verbose output" in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let sources =
  let doc = "Source file(s) to compile." in
  Arg.(value & pos_all string [] & info [] ~docv:"SOURCE" ~doc)
     
let version =
  let doc = "Display version number and exit" in
  Arg.(value & flag & info ["version"] ~doc)

(* Compiler options *)
let copts = "COMPILER OPTIONS"

let flex_only =
  let doc = "Run the lexical analyser only." in
  Arg.(value & flag & info ["flex-only"] ~doc ~docs:copts)
              
let frank_mode =
  let doc = Printf.sprintf "Strict Frank mode.\n\n This mode turns off features specific to Hank." in
  Arg.(value & flag & info ["frank"] ~doc ~docs:copts ~docv:"false")
     
(* Putting it all together *)
let cmd =
  let doc = "The Hank/Frank compiler" in
  let man = [
      `S "DESCRIPTION";
      `P "Files with \\fB.hk\\fR extensions are taken to be Hank source files, while files with extension \\fB.fk\\fR are taken to be Frank source files.";
      `S "BUGS";
      `P "Submit bug reports to <https://github.com/dhil/hank/issues>.";
      `S "AUTHOR";
      `P (Printf.sprintf "Written by Daniel Hillerström <%s@ed.ac.uk>" "daniel.hillerstrom")
    ]
  in
  Term.(ret (const handle_args
             $ verbose
             $ version
             $ sources
             $ frank_mode
             $ flex_only )),
  Term.info "hank" ~doc ~man

       
let args =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | `Ok args -> args
  | `Version | `Help -> exit 0
