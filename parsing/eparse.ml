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

module Multi : K = struct
  let apply k x =
    let k' = Obj.clone_continuation k in
    continue k' x
end

module Linear : K = struct
    let apply k x = continue k x
end

  
(** Primitive parsers **)
(** Parsers **)
module type PARSER = sig

  val satisfy : ('a -> bool) -> 'a
  val any  : unit -> 'a
  val fail : unit -> 'a
  val choose : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a

  val run : (unit -> 'a) -> 'a
end

module Parser : PARSER = struct

  effect Satisfy : ('a -> bool) -> 'a
  let satisfy p = perform (Satisfy p)
    
  effect Any : 'a
  let any () = perform Any

  effect Fail : 'a
  let fail : type a . unit -> a
     = fun () ->
       match perform Fail with | _ -> assert false
   
  effect Choose : (unit -> 'a ) * (unit -> 'a ) -> 'a
  let choose p q = fun () -> perform (Choose (p, q))

  let run : type a. (unit -> a) -> a
     = fun m ->
       let rec run' : type a. (unit -> a) -> a
         = fun m ->               
           match m () with
           | v -> v
           | effect Any k  -> failwith "Not yet implemented!"
           | effect Fail _ -> Opt.fail ()
           | effect (Choose (p,q)) k ->
              begin
                match Opt.optionalize (fun () -> run' p) with
                | Some v -> continue k v
                | None   ->
                   match Opt.optionalize (fun () -> run' q) with
                   | Some v -> continue k v
                   | None -> Opt.fail ()
              end
           | effect (Satisfy p) k -> failwith "Not yet implemented!"
       in
       run' m
end


module P = Parser

let char c = P.satisfy (fun c' -> c = c')
  
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
