(** Utils **)

type ordering = LT | EQ | GT

module Option: sig
  effect Fail : 'a
  val fail : unit -> 'a
  val optionalize : (unit -> 'a) -> 'a option
  val map : ('a -> 'b) -> 'a option -> 'b option
  val some : ('a -> 'b) -> ('a -> 'b option)
  val from : 'a option -> 'a
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

  let some f x = Some (f x)
  let from = function
    | Some x -> x
    | None -> fail ()
end

module Opt = Option


module Char = struct
  include Char

  let to_string c = Printf.sprintf "%c" c

  let is_uppercase c =
    let code = code c in
    code >= 65 && code <= 90

  let is_lowercase c =
    let code = code c in
    code >= 97 && code <= 122
end

module String = struct
  include String

  let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

  let implode l =
    let result = Bytes.create (List.length l) in
    let rec imp i = function
      | [] -> result
      | c :: l -> Bytes.set result i c; imp (i + 1) l in
    Bytes.to_string (imp 0 l)

  let of_char c =
    let b = Bytes.create 1 in
    Bytes.set b 0 c;
    Bytes.to_string b

  let is_capitalised s =
    if length s > 0 then
      Char.is_uppercase (get s 0)
    else
      false
end

module FP: sig
  val flip    : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
  val curry   : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
  val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)

  val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
  val (-<-)   : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
  val (->-)   : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
end = struct
  let flip : type a b c . (a -> b -> c) -> (b -> a -> c)
    = fun f y x -> f x y
  let curry : type a b c . (a * b -> c) -> (a -> b -> c)
    = fun f x y -> f (x,y)
  let uncurry : type a b c . (a -> b -> c) -> (a * b -> c)
    = fun f (x,y) -> f x y

  let compose : type a b c . (b -> c) -> (a -> b) -> (a -> c)
    = fun g f a -> f a |> g
  let (-<-) = compose
  let (->-) f g = compose g f
end

module List = struct
  include List

  let rec replicate x = function
    | n when n <= 0 -> []
    | n -> x :: (replicate x (n-1))

  let rec zipWith f xs ys =
    match xs, ys with
    | [], _ -> []
    | _, [] -> []
    | x :: xs, y :: ys -> let z = f x y in z :: zipWith f xs ys

  let zip xs ys = zipWith (fun x y -> (x,y)) xs ys
end

module type STREAM = sig
  type 'a t
  exception Failure

  val from : (int -> 'a option) -> 'a t
  val of_list : 'a list -> 'a t
  val of_string : string -> char t
  val of_bytes : bytes -> char t
  val of_channel : in_channel -> char t
  val iter : ('a -> unit) -> 'a t -> unit
  val next : 'a t -> 'a
  val empty : 'a t
  val peek : 'a t -> 'a option
  val npeek : int -> 'a t -> 'a list
  val tee : 'a t -> ('a t * 'a t)
end

module Stream : STREAM = struct
  type 'a t = 'a Stream.t

  exception Failure = Stream.Failure

  let from = Stream.from
  let of_list = Stream.of_list
  let of_string = Stream.of_string
  let of_bytes = Stream.of_bytes
  let of_channel = Stream.of_channel
  let iter = Stream.iter
  let next = Stream.next
  let empty = Stream.sempty
  let peek = Stream.peek
  let npeek = Stream.npeek

  let tee stream =
    let next self other i =
      try
        if Queue.is_empty self
        then
          let value = Stream.next stream in
          Queue.add value other;
          Some value
        else
          Some (Queue.take self)
      with Stream.Failure -> None in
    let q1 = Queue.create () in
    let q2 = Queue.create () in
    (Stream.from (next q1 q2), Stream.from (next q2 q1))
end

(* module Make (Ord : Map.OrderedType) = *)
(* struct *)
(*   include Map.Make(Ord) *)

(*   let from_list : 'a list -> 'a t *)
(*     = fun xs -> *)
(*     List.fold_left (fun map x -> Map.add x map) Map.empty xs *)
(* end *)

(* module StringMap = Make(String) *)
(* module IntMap = Make(struct *)
(*                       type t = int *)
(*                       let compare = Pervasives.compare *)
(*                     end) *)

