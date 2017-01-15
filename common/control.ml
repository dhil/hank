(*open Continuation*)
       
module type STATE = sig
  type t

  val get : unit -> t
  val put : t -> unit

  val run : (unit -> 'a) -> init:t -> ('a * t)
  val eval : (unit -> 'a) -> t -> 'a
end

module State(K : K)(S : sig type t end) : STATE with type t := S.t = struct
  type t = S.t

  effect Get : t
  let get () = perform Get
        
  effect Put : t -> unit
  let put s = perform (Put s)

  let run m ~init =
    let comp =
      match m () with
      | v -> (fun s -> (v, s))
      | effect (Put s) k -> (fun _ -> K.apply k () s)
      | effect Get k -> (fun s -> K.apply k s s)
    in
    comp init

  let eval m init = fst (run m ~init)
end

module type REF = sig
  type 'a t
  val ref : 'a -> 'a t
  val (:=) : 'a t -> 'a -> unit
  val (!) : 'a t -> 'a

  val run : (unit -> 'a) -> 'a
end

module Ref(K : K) : REF = struct
  module type T = sig
    type elt
    effect Get : elt
    effect Set : elt -> unit
  end

  type 'a t = (module T with type elt = 'a)

  effect Ref : 'a -> 'a t
  let ref v = perform (Ref v)
  let (!) : type a . a t -> a = fun (module R) -> perform R.Get
  let (:=) : type a . a t -> a -> unit = fun (module R) v -> perform (R.Set v)

  let run m =
    match m () with
    | v -> v
    | effect (Ref v) k ->
       (v, k) |> fun (type a) (v, k : a * (a t,_) continuation) ->
        let module R =
          struct
            type elt = a
            effect Get : elt
            effect Set : elt -> unit
          end
        in
        v |> begin match
               K.apply k (module R)
             with
             | v                  -> fun _ -> v
             | effect R.Get     k -> fun s -> K.apply k s s
             | effect (R.Set s) k -> fun _ -> K.apply k () s
             end
end
