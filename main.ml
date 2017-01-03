let filename = Sys.argv.(1)

let main () =
  let open Eparse in
  let input = open_in filename in
  Printf.printf "Hello World!\n";
  close_in input

let _ = main ()
