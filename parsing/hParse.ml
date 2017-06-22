open Continuation
open Utils


(** Primitive parsers **)
type ('t, 'a) parser = {
    parse: unit -> 'a;
  } (*[@unboxed]*)

exception SyntaxError

module type TOKEN = sig
  type t
  val compare : t -> t -> ordering
end

(** Parsers **)
module type COMBINATORS = sig
  type 'a t
  type token
  val succeed : 'a -> 'a t
  val epsilon : unit t
  val nil     : 'a list t
  val ( *>) : 'a t -> 'b t -> 'b t
  val (<*)  : 'a t -> 'b t -> 'a t
  val (<*>) : 'a t -> 'b t -> ('a * 'b) t
  val (<|>) : 'a t -> 'a t -> 'a t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  val many : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t
  val chainl : 'a t -> ('a -> 'a -> 'a) t -> 'a t
  val optional : 'a t -> 'a option t
  val one_of : token list -> token t
  val none_of : token list -> token t
  val enclosed : left:token -> right:token -> 'a t -> 'a t
  val separated_list : sep:token -> 'a t -> 'a list t
end

module type PARSER = sig
  type token
  type 'a t = (token Stream.t, 'a) parser

  val compare : token -> token -> ordering

  val of_comp : (unit -> 'a) -> 'a t
  val satisfy : (token -> bool) -> token t
  val any  : token t
  val token : token -> token t
  val fail : 'a t
  val choose : 'a t -> 'a t -> 'a t
  val eos  : unit t

  val eval : 'a t -> 'a
  val run : token Stream.t -> 'a t -> 'a

  include COMBINATORS with type 'a t := 'a t
                       and type token := token
end

module Parser(K : K)
             (Token : TOKEN)
       : PARSER with type token = Token.t =
struct
  type nonrec token = Token.t
  type nonrec 'a t = (Token.t Stream.t, 'a) parser

  let compare = Token.compare

  let of_comp : (unit -> 'a) -> 'a t
    = fun comp -> { parse = comp }

  let eval : 'a t -> 'a
    = fun p -> p.parse ()

  exception Fail
  let fail : type a . a t
    = of_comp (fun _ -> raise Fail)

  effect Any : Token.t
  let any : Token.t t
    = of_comp (fun () -> perform Any)

  let satisfy : (Token.t -> bool) -> Token.t t
    = fun p ->
    let parse () =
      let tok = perform Any in
      if p tok then tok
      else raise Fail
    in
    of_comp parse

  let token : Token.t -> Token.t t
    = fun t1 ->
    satisfy (fun t2 -> (Token.compare t1 t2) = EQ)

  effect Choose : 'a t * 'a t -> 'a
  let choose : type a. a t -> a t -> a t
    = fun p q ->
    of_comp (fun () -> perform (Choose (p,q)))

  effect EOS : unit
  let eos : unit t =
    of_comp (fun () -> perform EOS)

  let run : type a. Token.t Stream.t -> a t -> a
    = fun stream p ->
    let stream = ref stream in
    let rec run : type a. a t -> a
      = fun p ->
      match p.parse () with
      | result -> result
      | effect EOS k ->
         begin
           try
             ignore (Stream.peek !stream);
             K.raise k Fail
           with Stream.Failure -> K.apply k ()
         end
      | effect Any k ->
         begin
           try
             K.apply k (Stream.next !stream)
           with Stream.Failure -> K.raise k Fail
         end
      | effect (Choose (p,q)) k ->
         begin
           let (s1,s2) = Stream.tee !stream in
           match stream := s1; run p with
           | result -> K.apply k result
           | exception Fail ->
              match stream := s2; run q with
              | result -> K.apply k result
              | exception Fail -> K.raise k Fail
         end
      | exception Fail -> raise Fail
      | exception Stream.Failure -> failwith "Stream failure"
    in
    try
      run p
    with Fail -> raise SyntaxError

  let succeed v = of_comp (fun () -> v)
  let epsilon = succeed ()
  let nil = succeed []

  let ( *>) p q =
    let parse () =
      let _ = eval p in
      eval q
    in
    of_comp parse

  let (<*) p q =
    let parse () =
      let x = eval p in
      let _ = eval q in
      x
    in
    of_comp parse

  let (<*>) : type a b. a t -> b t -> (a * b) t
    = fun p q ->
    let parse () =
      let x = eval p in
      let y = eval q in
      (x, y)
    in
    of_comp parse

  let (<|>) : type a. a t -> a t -> a t
    = choose

  let (<$>) f p =
    let parse () =
      let x = eval p in
      f x
    in
    of_comp parse

  let rec many p =
    let lhs () =
      let x = eval p in
      let xs = eval (many p) in
      x :: xs
    in
    (of_comp lhs) <|> nil

  let rec many1 p =
    let parse () =
      let x = eval p in
      let xs = eval (many p) in
      x :: xs
    in
    of_comp parse

  let chainl p q =
    let f (x, xs) =
      List.fold_left (fun acc (op, y) -> op y acc) x xs
    in
    f <$> (p <*> (many (q <*> p)))

  let optional =
    fun p ->
    let lhs () =
      let x = eval p in
      Some x
    in
    (of_comp lhs) <|> (succeed None)

  let rec mem y = function
    | []      -> false
    | x :: xs when Token.compare x y = EQ -> true
    | _ :: xs -> mem y xs

  let one_of : Token.t list -> Token.t t =
    fun xs ->
    satisfy (fun y -> mem y xs)

  let none_of : Token.t list -> Token.t t =
    fun xs ->
    satisfy (fun y -> not @@ mem y xs)

  let enclosed : left:Token.t -> right:Token.t -> 'a t -> 'a t
    = fun ~left ~right p ->
    let parse () =
      let _ = eval @@ token left in
      let x = eval p in
      let _ = eval @@ token right in
      x
    in
    of_comp parse

  let separated_list : sep:Token.t -> 'a t -> 'a list t
    = fun ~sep p ->
    let px = p in
    let pxs = many p in
    let q = (fun (x,xs) -> x :: xs) <$> (px <*> pxs) in
    q <|> nil
end

(* (\** Higher order parsers **\) *)
(* module type COMBINATORS = sig *)
(*   include PARSER with type 'a t := 'a P.t *)
(*   val succeed : 'a -> 'a t *)
(*   val epsilon : unit t *)
(*   val nil     : 'a list t *)
(*   val ( *>) : 'a t -> 'b t -> 'b t *)
(*   val (<*\)  : 'a t -> 'b t -> 'a t *)
(*   val (<*>) : 'a t -> 'b t -> ('a * 'b) t *)
(*   val (<|>) : 'a t -> 'a t -> 'a t *)
(*   val (<$>) : ('a -> 'b) -> 'a t -> 'b t *)
(*   val many : 'a t -> 'a list t *)
(*   val many1 : 'a t -> 'a list t *)
(*   val chainl : 'a t -> ('a -> 'a -> 'a) t -> 'a t *)
(*   val optional : 'a t -> 'a option t *)
(*   (\* val sequence : s list -> (s, unit) parser *\) *)
(*   (\* val enclosed : s -> (s, 'a) parser -> s -> (s, 'a) parser *\) *)
(*                             (\*val one_of : Token.t list -> Token.t t*\) *)
(* end *)

(* module Combinators(P : PARSER) : COMBINATORS with type 'a t := 'a P.t and type token := P.token = *)
(* struct *)
(*   include P *)
(*   let succeed v = *)
(*     P.of_comp (fun () -> v) *)
(*   let epsilon = succeed () *)
(*   let nil = succeed [] *)

(*   let ( *>) p q = *)
(*     let parse () = *)
(*       let _ = P.eval p in *)
(*       P.eval q *)
(*     in *)
(*     P.of_comp parse *)

(*   let (<*\) p q = *)
(*     let parse () = *)
(*       let x = P.eval p in *)
(*       let _ = P.eval q in *)
(*       x *)
(*     in *)
(*     P.of_comp parse *)

(*   let (<*>) : type a b. a P.t -> b P.t -> (a * b) P.t *)
(*     = fun p q -> *)
(*     let parse () = *)
(*       let x = P.eval p in *)
(*       let y = P.eval q in *)
(*       (x, y) *)
(*     in *)
(*     P.of_comp parse *)

(*   let (<|>) : type a. a P.t -> a P.t -> a P.t *)
(*     = P.choose *)

(*   let (<$>) f p = *)
(*     let parse () = *)
(*       let x = P.eval p in *)
(*       f x *)
(*     in *)
(*     P.of_comp parse *)

(*   let rec many p = *)
(*     let lhs () = *)
(*       let x = P.eval p in *)
(*       let xs = P.eval (many p) in *)
(*       x :: xs *)
(*     in *)
(*     (P.of_comp lhs) <|> nil *)

(*   let rec many1 p = *)
(*     let parse () = *)
(*       let x = P.eval p in *)
(*       let xs = P.eval (many p) in *)
(*       x :: xs *)
(*     in *)
(*     P.of_comp parse *)

(*   let chainl p q = *)
(*     let f (x, xs) = *)
(*       List.fold_left (fun acc (op, y) -> op y acc) x xs *)
(*     in *)
(*     f <$> (p <*> (many (q <*> p))) *)

(*   let optional = *)
(*     fun p -> *)
(*     let lhs () = *)
(*       let x = P.eval p in *)
(*       Some x *)
(*     in *)
(*     (P.of_comp lhs) <|> (succeed None) *)

(*   let one_of : P.token list -> P.token P.t = *)
(*     let rec mem y = function *)
(*       | []      -> false *)
(*       | x :: xs when P.compare x y = EQ -> true *)
(*       | _ :: xs -> mem y xs *)
(*     in *)
(*     fun xs -> *)
(*     let y = P.eval P.any in *)
(*     if mem y xs then succeed y *)
(*     else P.fail *)

(*   (\* let sequence seq = *\) *)
(*   (\*   let parse s = *\) *)
(*   (\*     List.fold_left *\) *)
(*   (\*       (fun ((),s') x -> *\) *)
(*   (\*         let (y, s'') = run' s' P.any in *\) *)
(*   (\*         if x = y *\) *)
(*   (\*         then ((),s'') *\) *)
(*   (\*         else run' s' P.fail) ((),s) seq *\) *)
(*   (\*   in *\) *)
(*   (\*   mk_parser parse *\) *)

(*   (\* let enclosed l p r = *\) *)
(*   (\*   let p = *\) *)
(*   (\*     (P.satisfy (fun l' -> l = l')) *> p *\) *)
(*   (\*   in *\) *)
(*   (\*   p <* (P.satisfy (fun r' -> r = r')) *\) *)

(*   (\* let one_of ps = *\) *)
(*   (\*   List.fold_left (<|>) P.fail ps *\) *)

(*   (\* let none_of ps = failwith "Not yet implemented" *\) *)
(* end *)


(* module type CHARPARSER = sig *)
(*   include PARSER with type s := char *)
(*   include COMBINATORS with type s := char *)

(*   type 'a char_parser = (char, 'a) parser *)

(*   val peek_char : char -> unit char_parser *)
(*   val peek : (char -> bool) -> unit char_parser *)
(*   val char : char -> char char_parser *)
(*   val whitespace : unit char_parser *)
(*   val space : unit char_parser *)
(*   val spaces : unit char_parser *)
(*   val string : string -> string char_parser *)
(*   val digit : char char_parser *)
(*   val natural : string char_parser *)
(*   val signed : string char_parser -> string char_parser *)
(*   val integer : string char_parser *)
(* end *)

(* module CharParser(CP : PARSER with type s = char) : CHARPARSER = struct *)
(*   include CP *)
(*   include Combinators(CP) *)
(*   type 'a char_parser = (char, 'a) parser *)

(*   let run' s p = p.parse s *)

(*   let peek_char c = *)
(*     let parse s = *)
(*       match Stream.peek s with *)
(*       | Some c' when c = c' -> ((), s) *)
(*       | _ -> run' s CP.fail *)
(*     in *)
(*     mk_parser parse *)

(*   let peek f = *)
(*     let parse s = *)
(*       match Stream.peek s with *)
(*       | Some c when f c -> ((), s) *)
(*       | _ -> run' s CP.fail *)
(*     in *)
(*     mk_parser parse *)

(*   let char c = CP.satisfy (fun c' -> c = c') *)
(*   let whitespace = CP.satisfy (function *)
(*                                | ' ' | '\012' | '\n' | '\r' | '\t' -> true *)
(*                                | _ -> false) *)
(*                    *> epsilon *)
(*   let space = (CP.satisfy (function *)
(*                           | ' ' | '\t' -> true *)
(*                           | _ -> false) *)
(*               ) *> epsilon *)

(*   let spaces = (many space) *> epsilon *)

(*   let string str = *)
(*     let parse s = *)
(*       let (_,s') = run' s (sequence (String.explode str)) in *)
(*       (str,s') *)
(*     in *)
(*     mk_parser parse *)

(*   let digit = CP.satisfy (function *)
(*                           | '0' .. '9' -> true *)
(*                           | _          -> false) *)

(*   let natural = *)
(*     let parse s = *)
(*       let (ds, s') = run' s (many1 digit) in *)
(*       (String.implode ds, s') *)
(*     in *)
(*     mk_parser parse *)

(*   let signed p = *)
(*     let with_sign c s = *)
(*       let (str,s') = run' s ((char c) *> p) in *)
(*       (Bytes.of_string str *)
(*        |> (fun bs -> Bytes.extend bs 1 0) *)
(*        |> (fun bs -> Bytes.set bs 0 c; bs) *)
(*        |> Bytes.to_string *)
(*       , s') *)
(*     in *)
(*     let positive = mk_parser (fun s -> with_sign '+' s) in *)
(*     let negative = mk_parser (fun s -> with_sign '-' s) in *)
(*     positive <|> negative <|> p *)

(*   let integer = signed natural *)

(* end *)

(* module CP_s = Parser(Singleshot)(struct type t = char end) *)
(* module CP = CharParser(CP_s) *)
