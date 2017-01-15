

let parse src = failwith "Not yet implemented"

let main () =
  let filename = List.hd Cmdline.(args.sources) in
  let sources  = List.map Source.make Cmdline.(args.sources) in
  let open Eparse in
  let input = open_in filename in
  Printf.printf "Hello World!\n";
  close_in input

let _ = main ()
