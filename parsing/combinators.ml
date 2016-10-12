(** Parser combinators with effect handlers. 
    Inspired by Lindley (2014) **)

type zero = {_z : 'a. 'a}
  
effect parse = Any : unit -> char
  | Char : char -> char
  | Fail : zero
  | Choose : bool

let any () = perform Any ()
let char' c = perform Char c
let fail : 'a . unit -[parse]-> 'a =
  fun () ->
    match perform Fail with
    | _ -> assert false
    
let choose : 'a. 'a ->> 'a -> unit -[parse]-> 'a =
  fun x y ()  ->
    if perform Choose
    then x
    else y
