(** Utils **)
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
      | c :: l -> result.[i] <- c; imp (i + 1) l in
    Bytes.to_string (imp 0 l)

  let of_char c =
    let b = Bytes.create 1 in
    b.[0] <- c;
    Bytes.to_string b
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
