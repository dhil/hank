open Utils
open Eparse

let rest stream =
  let xs = ref [] in
  try
    while true do
      let x = Stream.next stream in
      xs := x :: !xs
    done;
    ""
  with | Stream.Failure -> String.implode (List.rev !xs)
          
let runp : type a . a parser -> string -> (a * string)
  = fun p s ->
  let stream = Stream.of_list (String.explode s) in
  let r = CP.run stream p in
  r, rest stream

let runp' p s = fst (runp p s)

let tests n =
  let char =
    QCheck.Test.make ~count:n
                     ~name:"char"
                     QCheck.(printable_char)
                     (fun c -> let r = runp' CP.(char c) (String.of_char c) in
                               r = c)
  in
  let digit =
    QCheck.Test.make ~count:n
                     ~name:"digit"
                     QCheck.(numeral_char)
                     (fun i -> let r = runp' CP.digit (String.of_char i) in
                               r = i)
  in
  let natural =
    QCheck.Test.make ~count:n
                     ~name:"natural"
                     QCheck.(int)
                     (fun i -> let i = string_of_int (abs i) in
                               let r = runp' CP.natural i in
                               String.compare r i = 0)
  in
  let spaces_natural =
    let make_space i = Bytes.to_string (Bytes.make i ' ') in
    QCheck.Test.make ~count:n
                     ~name:"spaces_natural"
                     QCheck.(int)
                     (fun i -> let i = abs i in
                               let j = string_of_int n in
                               let r = runp' CP.(spaces *> natural) (Printf.sprintf "%s%s" (make_space i) j) in
                               String.compare r j = 0)
  in
  let string =
    QCheck.Test.make ~count:n
                     ~name:"string"
                     QCheck.(printable_string)
                     (fun s -> let r = runp' CP.(string s) s in
                               String.compare s r = 0)
  in
  let natural_seq =
    QCheck.Test.make ~count:n
                     ~name:"natural_seq"
                     QCheck.(int)
                     (fun i -> let i = abs i in
                               let s = Printf.sprintf "%d;%d" i i in
                               let (x,y) = runp' CP.( (natural <* char ';') <*> natural ) s in
                               int_of_string x = i && i = int_of_string y)
  in
  [ char
  ; digit
  ; natural
  (* ; spaces_natural *)
  ; string
  ; natural_seq ]
