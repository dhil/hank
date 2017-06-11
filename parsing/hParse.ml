open Continuation
open Utils


(** Primitive parsers **)
type ('s,'a) parser = {
    parse: 's Stream.t -> ('a * 's Stream.t);
  }

let mk_parser p = { parse = p }

exception SyntaxError

(** Parsers **)
module type PARSER = sig
  type s

  val satisfy : (s -> bool) -> (s, s) parser
  val any  : (s, s) parser
  val fail : (s, 'a) parser
  val choose : (s, 'a) parser -> (s, 'a) parser -> (s, 'a) parser
  val eos  : (s, unit) parser

  val run : s Stream.t -> (s, 'a) parser -> 'a
end

module Parser(K : K)(S : sig type t end) : PARSER with type s = S.t = struct
  type s = S.t

  effect Fail : 'a
  let fail : type a . (s, a) parser
    = mk_parser
        (fun _ ->
          match perform Fail with | _ -> assert false)

  let satisfy p =
    let parse s =
      try
        match Stream.next s with
        | v when p v -> (v, s)
        | _ -> perform Fail
      with Stream.Failure -> perform Fail
    in
    mk_parser parse

  effect Choose : (s, 'a) parser * (s, 'a) parser -> ('a * s Stream.t)
  let choose : type a. (s, a) parser -> (s, a) parser -> (s, a) parser
    = fun p q ->
    let parse s =
      let (s1, s2) = Stream.tee s in
      match perform (Choose (p,q)) with
      | p' -> p'
      | effect (Choose (p,q)) k ->
         begin
           match p.parse s1 with
           | res -> K.apply k res
           | effect Fail _ ->
              match q.parse s2 with
              | res -> K.apply k res
              | effect Fail _ -> perform Fail
         end
    in
    mk_parser parse

  let any =
    let parse s =
      try
        let v = Stream.next s in
        (v, s)
      with Stream.Failure -> perform Fail
    in
    mk_parser parse

  let eos =
    let parse s =
      match Stream.peek s with
      | None -> (), Stream.empty
      | _    -> perform Fail
    in
    mk_parser parse

  let run : type a. s Stream.t -> (s, a) parser -> a
    = fun stream p ->
    match p.parse stream with
    | (v,_) -> v
    | effect Fail _ -> raise SyntaxError
    | exception Stream.Failure -> failwith "Stream failure"
end

(** Higher order parsers **)
module type COMBINATORS = sig
  type s
  val ( *>) : (s, 'a) parser -> (s, 'b) parser -> (s, 'b) parser
  val (<*)  : (s, 'a) parser -> (s, 'b) parser -> (s, 'a) parser
  val (<*>) : (s, 'a) parser -> (s, 'b) parser -> (s, 'a * 'b) parser
  val (<|>) : (s, 'a) parser -> (s, 'a) parser -> (s, 'a) parser
  val (<$>) : ('a -> 'b) -> (s, 'a) parser -> (s, 'b) parser
  val many  : (s, 'a) parser -> (s, 'a list) parser
  val many1 : (s, 'a) parser -> (s, 'a list) parser
  val optional : (s, 'a) parser -> (s, 'a option) parser
  val succeed : 'a -> (s, 'a) parser
  val epsilon : (s, unit) parser
  val chainl : (s,'a) parser -> (s, 'a -> 'a -> 'a) parser -> (s, 'a) parser
  val sequence : s list -> (s, unit) parser
  val enclosed : s -> (s, 'a) parser -> s -> (s, 'a) parser
  val one_of : (s, 'a) parser list -> (s, 'a) parser
end

module Combinators (P : PARSER) : COMBINATORS with type s := P.s = struct
  type s = P.s
  let run' (s : s Stream.t) p = p.parse s

  let ( *>) p q =
    let parse s =
      let (_,s') = run' s p in
      run' s' q
    in
    mk_parser parse

  let (<*) p q =
    let parse s =
      let (x,s') = run' s p in
      let (_,s'') = (run' s' q) in
      (x,s'')
    in
    mk_parser parse

  let (<*>) : type a b. (s, a) parser -> (s, b) parser -> (s, a * b) parser
    = fun p q ->
    let parse s =
      let (x,s') = run' s p in
      let (y,s'') = run' s' q in
      ((x,y), s'')
    in
    mk_parser parse

  let (<|>) : type a. (s, a) parser -> (s, a) parser -> (s, a) parser
    = P.choose

  let (<$>) f p =
    let parse s =
      let (x, s') = run' s p in
      (f x, s')
    in
    mk_parser parse

  let succeed v =
    mk_parser (fun (s : s Stream.t) -> (v, s))

  let nil = succeed []
  let epsilon = succeed ()

  let rec many p =
    let lhs =
      mk_parser
        (fun s ->
          let (x,s') = run' s p in
          let (xs,s'') = run' s' (many p) in
          (x :: xs, s''))
    in
    lhs <|> nil

  let many1 p =
    let parse s =
      let (x,s') = run' s p in
      let (xs,s'') = run' s' (many p) in
      (x :: xs, s'')
    in
    mk_parser parse

  let chainl p q =
    let f (x, xs) = List.fold_left (fun acc (op,y) -> op y acc) x xs in
      f <$> (p <*> (many (q <*> p)))

  let optional p =
    let lhs =
      mk_parser
        (fun s ->
          let (x, s') = run' s p in
          (Some x, s'))
    in
    lhs <|> (succeed None)

  let sequence seq =
    let parse s =
      List.fold_left
        (fun ((),s') x ->
          let (y, s'') = run' s' P.any in
          if x = y
          then ((),s'')
          else run' s' P.fail) ((),s) seq
    in
    mk_parser parse

  let enclosed l p r =
    let p =
      (P.satisfy (fun l' -> l = l')) *> p
    in
    p <* (P.satisfy (fun r' -> r = r'))

  let one_of ps =
    List.fold_left (<|>) P.fail ps

  let none_of ps =
    ((one_of ps) <* P.fail) <|> any
end


module type CHARPARSER = sig
  include PARSER with type s := char
  include COMBINATORS with type s := char

  type 'a char_parser = (char, 'a) parser

  val peek_char : char -> unit char_parser
  val peek : (char -> bool) -> unit char_parser
  val char : char -> char char_parser
  val whitespace : unit char_parser
  val space : unit char_parser
  val spaces : unit char_parser
  val string : string -> string char_parser
  val digit : char char_parser
  val natural : string char_parser
  val signed : string char_parser -> string char_parser
  val integer : string char_parser
end

module CharParser(CP : PARSER with type s = char) : CHARPARSER = struct
  include CP
  include Combinators(CP)
  type 'a char_parser = (char, 'a) parser

  let run' s p = p.parse s

  let peek_char c =
    let parse s =
      match Stream.peek s with
      | Some c' when c = c' -> ((), s)
      | _ -> run' s CP.fail
    in
    mk_parser parse

  let peek f =
    let parse s =
      match Stream.peek s with
      | Some c when f c -> ((), s)
      | _ -> run' s CP.fail
    in
    mk_parser parse

  let char c = CP.satisfy (fun c' -> c = c')
  let whitespace = CP.satisfy (function
                               | ' ' | '\012' | '\n' | '\r' | '\t' -> true
                               | _ -> false)
                   *> epsilon
  let space = (CP.satisfy (function
                          | ' ' | '\t' -> true
                          | _ -> false)
              ) *> epsilon

  let spaces = (many space) *> epsilon

  let string str =
    let parse s =
      let (_,s') = run' s (sequence (String.explode str)) in
      (str,s')
    in
    mk_parser parse

  let digit = CP.satisfy (function
                          | '0' .. '9' -> true
                          | _          -> false)

  let natural =
    let parse s =
      let (ds, s') = run' s (many1 digit) in
      (String.implode ds, s')
    in
    mk_parser parse

  let signed p =
    let with_sign c s =
      let (str,s') = run' s ((char c) *> p) in
      (Bytes.of_string str
       |> (fun bs -> Bytes.extend bs 1 0)
       |> (fun bs -> Bytes.set bs 0 c; bs)
       |> Bytes.to_string
      , s')
    in
    let positive = mk_parser (fun s -> with_sign '+' s) in
    let negative = mk_parser (fun s -> with_sign '-' s) in
    positive <|> negative <|> p

  let integer = signed natural

end

(* module CP_s = Parser(Singleshot)(struct type t = char end) *)
(* module CP = CharParser(CP_s) *)
