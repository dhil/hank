(** Utils **)
module Option: sig
  val fail : unit -> 'a
  val optionalize : (unit -> 'a) -> 'a option
  val map : ('a -> 'b) -> 'a option -> 'b option
end = struct
  effect Fail : 'a
  let fail () = match perform Fail with | _ -> assert false

  let optionalize m =
    match m () with
    | v             -> Some v
    | effect Fail _ -> None
  

  let map f = function
    | Some x -> Some (f x)
    | None   -> None
end

module Opt = Option

module type K = sig
    val apply : ('a, 'b) continuation -> 'a -> 'b
end

module Multishot : K = struct
  let apply k x =
    let k' = Obj.clone k in
    continue k' x
end

module Singleshot : K = struct
    let apply k x = continue k x
end

  
(** Primitive parsers **)
type 'a parser = unit -> 'a
let runp : type a. a parser -> a
  = fun p -> p ()
               
(** Parsers **)
module type PARSER = sig
  type t

  val satisfy : (t -> bool) -> t parser
  val any  : t parser
  val fail : 'a parser
  val choose : 'a parser -> 'a parser -> 'a parser
  val eos  : unit parser

  val run : t Stream.t -> 'a parser -> 'a
end

module Parser(K : K)(S : sig type t end) : PARSER with type t = S.t = struct
  type t = S.t
                                                                    
  effect Satisfy : (t -> bool) -> t
  let satisfy p () = perform (Satisfy p)
    
  let any = satisfy (fun _ -> true)

  effect Fail : 'a
  let fail : type a . unit -> a
     = fun () ->
       match perform Fail with | _ -> assert false
   
  effect Choose : 'a parser * 'a parser -> 'a
  let choose p q () = perform (Choose (p, q))

  effect EOS : unit
  let eos () = perform EOS
                           
  let run : type a. t Stream.t -> a parser -> a
    = fun inp m ->
    let getc = fun () -> Stream.next inp in
    let peek = fun () -> Stream.peek inp in
    let rec run' : type a. a parser -> a
    = fun m ->
    try 
      match m () with
      | v -> v
      | effect EOS k  ->
         begin
           try Stream.empty inp; K.apply k ()
           with Stream.Failure -> fail ()
         end
      | effect (Satisfy p) k ->
         begin
           let c = peek () in
           match c with
           | Some c -> if p c then K.apply k (getc ()) else fail ()
           | None   -> fail ()
         end
      | effect (Choose (p,q)) k ->
         begin
           match Opt.optionalize (fun () -> run' p) with
           | Some v -> K.apply k v
           | None   ->
              match Opt.optionalize (fun () -> run' q) with
              | Some v -> K.apply k v
              | None -> fail ()
         end
    with
    | effect Fail _ -> Opt.fail ()
    in
    run' m
end

(** Higher order parsers **)
module type COMBINATORS = sig
  val ( *>) : 'a parser -> 'b parser -> 'b parser
  val (<*)  : 'a parser -> 'b parser -> 'a parser
  val (<*>) : 'a parser -> 'b parser -> ('a * 'b) parser
  val (<|>) : 'a parser -> 'a parser -> 'a parser
  val many : 'a parser -> 'a list parser
  val many1 : 'a parser -> 'a list parser
  val empty : unit parser
end
                             
module Combinators (P : PARSER) : COMBINATORS = struct
  let ( *>) p q () = ignore (runp p); runp q
  let (<*)  p q () = let x = runp p in ignore (runp q); x
  let (<*>) p q () = let x = runp p in let y = runp q in (x, y) 
  let (<|>) p q = P.choose p q
  let rec many p = (fun () -> let x = runp p in let rest = runp (many p) in x :: rest) <|> (fun () -> [])
  let many1 p () = let x = runp p in let rest = runp (many p) in x :: rest
  let empty () = ()
end

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let implode l =
  let result = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  Bytes.to_string (imp 0 l)
                                                   
module type CHARPARSER = sig
  include PARSER with type t = char
  include COMBINATORS

                                  
  val char : char -> char parser
  val whitespace : unit parser
  val space : unit parser
  val spaces : unit parser
  val string : string -> string parser
  val digit : unit -> char
  val natural : unit -> string
  val signed : string parser -> string parser
  val integer : string parser
end
                                                   
module CharParser(CP : PARSER with type t = char) : CHARPARSER = struct
  include CP
  include Combinators(CP)
  
                     
  let char c = CP.satisfy (fun c' -> c = c')
  let whitespace = CP.satisfy (function
                               | ' ' | '\012' | '\n' | '\r' | '\t' -> true
                               | _ -> false)
                   *> empty
  let space = CP.satisfy (function
                          | ' ' | '\t' -> true
                          | _ -> false)
              *> empty
                      
  let spaces = (many space) *> empty
  let string s = fun () -> String.map (fun c -> runp (char c)) s
  let digit = CP.satisfy (function
                          | '0' .. '9' -> true
                          | _          -> false)
                           
  let natural () = implode @@ many1 digit ()
  let signed p =
    let with_sign c () = ignore (runp (char c));
                       Bytes.of_string (runp p)
                       |> (fun bs -> Bytes.extend bs 1 0)
                       |> (fun bs -> Bytes.set bs 0 c; bs)
                       |> Bytes.to_string
    in
    let positive = with_sign '+' in
    let negative = with_sign '-' in
    positive <|> negative <|> p
  let integer = signed natural

                         
end
                                                   
module CP = Parser(Singleshot)(struct type t = char end)
module CP = CharParser(CP)
