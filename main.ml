
type 'a comp_unit =
  { filename: string
  ; source: 'a
  }

let parse src = failwith "Not yet implemented"

let main () =
  let filename = List.hd Cmdline.(args.sources) in
  let open Eparse in
  let input = open_in filename in
  Printf.printf "Hello World!\n";
  close_in input

let _ = main ()
