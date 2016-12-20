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
(** Parsers **)
module type PARSER = sig
  type t
         
  val satisfy : (t -> bool) -> t
  val any  : unit -> t
  val fail : unit -> 'a
  val choose : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a

  val run : t Stream.t -> (unit -> 'a) -> 'a
end

module Parser(K : K)(S : sig type t end) : PARSER with type t := S.t = struct
  type t = S.t
                                                                    
  effect Satisfy : (t -> bool) -> t
  let satisfy p = perform (Satisfy p)
    
  effect Any : t
  let any () = perform Any

  effect Fail : 'a
  let fail : type a . unit -> a
     = fun () ->
       match perform Fail with | _ -> assert false
   
  effect Choose : (unit -> 'a) * (unit -> 'a) -> 'a
  let choose p q = fun () -> perform (Choose (p, q))

  let run : type a. t Stream.t -> (unit -> a) -> a
    = fun inp m ->
    let getc = fun () -> Stream.next inp in
    let rec run' : type a. (unit -> a) -> a
       = fun (type b) m ->               
         match m () with
         | v -> v
         | effect Any k  -> failwith "Not yet implemented"
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


module Combinators =
  functor (P : PARSER) ->
  struct
   type t = P.t
   let rec many p = P.choose (fun () -> p () :: many p ()) (fun () -> [])                 
  end

module CP = Parser(Singleshot)(struct type t = char end)

let char c = CP.satisfy (fun c' -> c = c')
  
(* module UP = Parser(struct type t = unit end) *)
(* module LP = Parser(struct type 'a t = 'a list end) *)

(* let char' c = fun () -> let _ = CP.char c () in () *)
  
(* let (<|>) p q = CP.choose p q   *)
(* let digit = CP.(char' '0' <|> char' '1' <|> char' '3') *)

(* let choose (type a) (p : (unit -> a)) (q : (unit -> a)) = *)
(*   let module M = struct effect Choose : (unit -> a) * (unit -> a) -> a end in *)
(*   let open M in *)
(*   let rec handle m = *)
(*     match m () with *)
(*     | v -> v *)
(*     | effect (Choose (p,q)) k -> *)
(*        match handle p with *)
(*        | v -> continue k v *)
(*        | None -> *)
(*           match Opt.optionalize (fun () -> handle q) with *)
(*           | Some v -> continue k v *)
(*           | None   -> Opt.fail () *)
(*   in *)
(*   handle (fun () -> perform (Choose (p, q))) *)
