(* An abstraction on top of the lexer. *)

(* module Layout = struct
 *   type t = {
 *       mutable indents: int list;
 *       mutable tokenbuffer: Token.t list
 *     }
 * 
 *   let layout : Lexing.lexbuf -> 
 * end *)

type t = {
  lexbuf: Lexing.lexbuf;
  ic: in_channel;
  mutable buffer: Token.t list;
  mutable prev: Token.t option
}

let make lexbuf ic = { lexbuf; ic; buffer = []; prev = None }

let from_file : string -> t
  = fun file ->
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  make lexbuf ic

let close : t -> unit
  = fun scanner ->
  close_in scanner.ic;
  scanner.buffer <- [];
  scanner.prev <- None

let close_noerr : t -> unit
  = fun scanner ->
  close_in_noerr scanner.ic;
  scanner.buffer <- [];
  scanner.prev <- None

let recall : t -> Token.t option
  = fun scanner -> scanner.prev

let remember : t -> Token.t -> unit
  = fun scanner tok -> scanner.prev <- Some tok

let forget : t -> unit
  = fun scanner -> scanner.prev <- None

let push_back : t -> Token.t -> unit
  = fun scanner tok ->
  scanner.buffer <- tok :: scanner.buffer

let interpret : t -> Token.t -> Token.t
  = fun scanner tok ->
  let open Token in
  match tok with
  | BEGIN ->
     forget scanner; tok
  | _ ->
     begin match recall scanner with
     | Some LET | Some WHERE | Some OF ->
        forget scanner;
        begin match tok with
        | EOF ->
           remember scanner tok;
           INDENT 0
        | _ -> tok
        end
     | _ ->
        let indent = INDENT (Lexer.column scanner.lexbuf) in
        remember scanner tok; indent
     end

let next : t -> Token.t
  = fun scanner ->
  match scanner.buffer with
  | [] ->
     interpret scanner (Lexer.read scanner.lexbuf)
  | tok :: toks ->
     scanner.buffer <- toks; tok

