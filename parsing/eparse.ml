(** Utils **)
module Optionalize: sig
  val fail : unit -> 'a
  val optionalize : (unit -> 'a) -> 'a option
end = struct
  effect Fail : 'a
  let fail () = match perform Fail with | _ -> assert false

  let optionalize m =
    match m () with
    | v             -> Some v
    | effect Fail _ -> None
    | exception _   -> None
end

module Opt = Optionalize

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
type ('a, 'b) parser = 'a -> 'b
(** Parsers **)
module type PARSER = sig
  type t
         
  val satisfy : (t -> bool, t) parser
  val any  : (unit, t) parser
  val fail : (unit, 'a) parser
  val choose : (unit -> 'a) -> (unit -> 'a) -> 'a
  val eos  : (unit, unit) parser

  val run : t Stream.t -> (unit, 'a) parser -> 'a
end

module Parser(K : K)(S : sig type t end) : PARSER with type t := S.t = struct
  type t = S.t
                                                                    
  effect Satisfy : (t -> bool) -> t
  let satisfy p = perform (Satisfy p)
    
  let any () = satisfy (fun _ -> true)

  effect Fail : 'a
  let fail : type a . unit -> a
     = fun () ->
       match perform Fail with | _ -> assert false
   
  effect Choose : (unit -> 'a) * (unit -> 'a) -> 'a
  let choose p q = perform (Choose (p, q))

  effect EOS : unit
  let eos () = perform EOS
                           
  let run : type a. t Stream.t -> (unit, a) parser -> a
    = fun inp m ->
    let getc = fun () -> Stream.next inp in
    let rec run' : type a. (unit -> a) -> a
       = fun (type b) m ->               
         match m () with
         | v -> v
         | effect EOS k  -> try Stream.empty inp; continue k () with Stream.Failure -> Opt.fail ()
         | effect Fail _ -> Opt.fail ()
         | effect (Choose (p,q)) k ->
            begin
              match Opt.optionalize (fun () -> run' p) with
              | Some v -> K.apply k v
              | None   ->
                 match Opt.optionalize (fun () -> run' q) with
                 | Some v -> K.apply k v
                 | None -> Opt.fail ()
            end
         | effect (Satisfy p) k -> let c = getc () in if p c then continue k c else fail ()
    in
    run' m
end

(** Higher order parsers **)
module type COMBINATORS = sig
  val (<|>) : (unit, 'a) parser -> (unit, 'a) parser -> (unit, 'a) parser
  val many : (unit -> 'a) -> unit -> 'a list
  val many1 : (unit -> 'a) -> unit -> 'a list
  val option : (unit, 'a) parser -> (unit, 'a option) parser
end
                             
module Combinators (P : PARSER) : COMBINATORS  = struct
  let (<|>) p q = fun () -> P.choose p q
  let rec many p = (fun () -> p () :: many p ()) <|> (fun () -> [])
  let many1 p () = p () :: many p ()
  let option p () = failwith "option parser not implemented yet"
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
  include PARSER with type t := char
  include COMBINATORS

                                  
  val char : char -> (unit, char) parser
  val whitespace : (unit, unit) parser
  val string : string -> (unit, string) parser
  val digit : unit -> char
  val natural : unit -> string
  val signed : (unit, string) parser -> (unit, string) parser
  val integer : (unit, string) parser
end
                                                   
module CharParser (CP : PARSER with type t = char) : CHARPARSER = struct
  include CP
  include Combinators(CP)
  
                     
  let char c = fun () -> CP.satisfy (fun c' -> c = c')
  let whitespace () = ignore @@ CP.satisfy
                        (function
                         | ' ' | '\012' | '\n' | '\r' | '\t' -> true
                         | _ -> false)
  let string s = fun () -> String.map (fun c -> char c ()) s
  let digit () = CP.satisfy
                   (function
                    | '0' .. '9' -> true
                    | _          -> false)
  let natural () = implode @@ many1 digit ()
  let signed p =
    let with_sign c () = ignore (char c ());
                       Bytes.of_string (p ())
                       |> (fun bs -> Bytes.extend bs 1 0)
                       |> (fun bs -> Bytes.set bs 0 c; bs)
                       |> Bytes.to_string
    in
    let positive : (unit, string) parser = with_sign '+' in
    let negative = with_sign '-' in
    positive <|> negative <|> p
  let integer = signed natural

                         
end
                                                   
(*module CP = Parser(Singleshot)(struct type t = char end)

let char c = CP.satisfy (fun c' -> c = c')*)
