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
  
(** Parsers **)

module type PARSER = sig
  type t
  val char : char -> unit -> char
  val any  : unit -> char  
  val fail : unit -> 'a
  val choose : (< h : 'a . ((unit -> 'a) * (unit -> 'a)) >) -> (< g : 'a >)
    
  val parse : char Stream.t -> (unit -> 'a) -> 'a
end

module Parser (S : sig type t end) : PARSER with type t := S.t = struct
  type t = S.t
      
  effect Char : char -> char
  let char c = fun () -> perform (Char c)

  effect Any : char
  let any () = perform Any

  effect Fail : 'a
  let fail () = match perform Fail with | _ -> assert false
   
  effect Choose : (< h : 'a . ((unit -> 'a) * (unit -> 'a)) >) -> (< g : 'a >)
  let choose o = perform (Choose o)
   
  let parse inp p =
    let getc : unit -> char = fun () -> Stream.next inp in
    let rec run p () =
      match p () with
      | v -> v
      | effect (Choose o) k ->
         let p = fst o#h in
         match Opt.optionalize (run p) with
         | Some v -> continue k (object method g = v end)
         | _ -> failwith "Not yet implemented"
    in
    run p ()
    (* let rec run p () = *)
    (*   try  *)
    (*     match p () with *)
    (*     | v -> v *)
    (*     | effect (Char c) k -> *)
    (*        if c == getc () then continue k c *)
    (*        else Opt.fail () *)
    (*     | effect Any k -> *)
    (*        continue k (getc ()) *)
    (*     | effect Fail k -> Opt.fail () *)
    (*     | effect (Choose o) k -> *)
    (*        let p = fst o#g in *)
    (*        match optionalize (run p) with *)
    (*        | Some v -> continue k (wrap 2) *)
    (*        | _ -> assert false *)
    (*     (\* | effect (Choose (p', q)) k -> *\) *)
    (*     (\*    match Opt.optionalize (run p') with *\) *)
    (*     (\*    | Some (W v) -> continue k (W v) *\) *)
    (*     (\*    | None   -> failwith "" *\) *)
    (*           (\* begin *\) *)
    (*           (\*   match Opt.optionalize (run q) with *\) *)
    (*           (\*   | Some v -> continue k v *\) *)
    (*           (\*   | None   -> Opt.fail () *\) *)
    (*           (\* end *\) *)
    (*        with *)
    (*        | Stream.Failure -> Opt.fail () *)
    (* in *)
    (* run p () *)
                                
end


module CP = Parser(struct type t = unit end)

(* let char' c = fun () -> let _ = CP.char c () in () *)
  
(* let (<|>) p q = CP.choose p q   *)
(* let digit = CP.(char' '0' <|> char' '1' <|> char' '3') *)
