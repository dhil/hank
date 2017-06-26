open Utils

type name = string

type decl =
  | Sdecl_data of data
and data = {
     sdat_name: name;
     sdat_params: tyvar list;
     sdat_constrs: tyconstr list }
and tyvar = {
    styvar_name: name;
    styvar_kind: kind }
and kind =
  | Sk_eff (* effect type kind *)
  | Sk_val (* value type kind *)
and tyconstr = {
    stycon_name: name;
    stycon_params: value_type list }
and value_type =
  | Svtyp_var of tyvar
  | Svtyp_constr of tyconstr
  | Svtyp_base of base_type
and base_type = {
    sbty_kind: base_type_kind;
  }
and base_type_kind =
  | Int

(* type kind = *)
(*   | Sk_eff  (\* effect type kind *\) *)
(*   | Sk_val  (\* value type kind *\) *)

(* type value_type = *)
(*   | Svty_constr of tyconstr *)
(*   | Svty_comp of computation_type *)
(*   | Svty_var of type_var *)
(*   | Svty_int *)
(*   | Svty_string *)
(*   | Svty_char *)
(* and type_var = *)
(*   { styvar_name: name; *)
(*     styvar_kind: kind } *)
(* and type_arg = *)
(*   | Styarg_val of value_type *)
(*   | Styarg_eff of ability *)
(* and ability = type_var * type_arg list *)
(* and adjustment = unit list (\*type_arg StringMap.t (\* mapping from identifiers to type arguments *\)*\) *)
(* and computation_type = *)
(*   { sct_ports: port list; *)
(*     sct_peg: peg; } *)
(* and port = *)
(*   { sport_adjustment: adjustment; *)
(*     sport_type: value_type } *)
(* and peg = *)
(*   { speg_ability: ability; *)
(*     speg_type: value_type } *)
(* and tyconstr = { *)
(*     styco_name: name; *)
(*     styco_params: type_arg list *)
(* } *)

(* type decl = *)
(*   | Sdecl_data of data *)
(*   | Sdecl_interface of iface *)
(* and data = { *)
(*     sdat_name: name; *)
(*     sdat_tyargs: type_arg list; *)
(*     sdat_constrs: constr list } *)
(* and iface = { *)
(*     sif_name: name; *)
(*     sif_tyargs: type_arg list; *)
(*     sif_constrs: constr list } *)
(*   } *)


(* type term = *)
(*   | Sterm_use of use *)
(*   | Sterm_con of construction *)
(* and use = *)
(*   | Suse_mvar of name *)
(*   | Suse_pvar of name *)
(*   | Suse_cmd of name *)
(*   | Suse_app of use * construction list *)
(* and construction = *)
(*   | Scon_use of use *)
(*   | Scon_constr of name * construction list *)
(*   | Scon_scomp of clause list *)
(*   | Scon_let of use * construction * construction *)
(*   | Scon_letrec of (use * computation) list * construction *)
(* and computation = unit *)
(* and computation_pattern = *)
(*   | Scpat_val of value_pattern *)
(*   | Scpat_shallow of name * value_pattern list * value_pattern *)
(* and value_pattern = *)
(*   | Svpat_any *)
(*   | Svpat_var of name *)
(*   | Svpat_constr of name * value_pattern list *)

module Make = struct
  module Data = struct
    let mkdata : name -> tyvar list -> tyconstr list -> data
      = fun name params constrs ->
      { sdat_name = name;
        sdat_params = params;
        sdat_constrs = constrs }
  end
  module Types = struct
    module Value_type = struct
      let mkconstr : tyconstr -> value_type
        = fun tyconstr -> Svtyp_constr tyconstr

      let mktyconstr : name -> value_type list -> tyconstr
        = fun name args -> { stycon_name = name; stycon_params = args }
      (*let mkcomp : computation_type -> value_type
        = fun comp -> Svtyp_comp comp*)

      let mkvar : tyvar -> value_type
        = fun var -> Svtyp_var var

      let mktyvar : kind -> name -> tyvar
        = fun kind name -> { styvar_name = name; styvar_kind = kind }
    end

(*    module Computation_type = struct
      let mkcomp : port list -> peg -> computation_type
        = fun ports peg -> { ct_ports = ports; ct_peg = peg }

      let mkport : adjustment -> value_type -> port
        = fun adj typ -> { port_adjustment = adj; port_type = typ }

      let mkpeg : ability -> value_type -> peg
        = fun ab typ -> { peg_ability = ab; peg_type = typ }
    end

    module Row_type = struct
      let mkamb : 
    end *)
  end
end

module Tok = struct
  open Loc
  type t = Token.t Located.t
  let to_string t = Token.to_string @@ Located.item t
  let compare t1 t2 =
    let _ =
      Printf.printf "Comparing %s and %s\n" (to_string t1) (to_string t2);
    in
    Token.compare (Located.item t1) (Located.item t2)

  let lift : Token.t -> t
    = fun t -> Loc.Located.lift_dummy t

  let unpack : t -> Token.t
    = fun t -> Loc.Located.item t
end


open HParse
module Language (P : PARSER with type token = Tok.t) = struct
  open P
  open Token

  let lift = Tok.lift
  let parse = eval

  let brackets p =
    enclosed ~left:(lift LBRACKET) ~right:(lift RBRACKET) p

  let parens p =
    enclosed ~left:(lift LPAREN) ~right:(lift RPAREN) p

  let braces p =
    enclosed ~left:(lift LBRACE) ~right:(lift RBRACE) p

  let name =
    let ident =
      satisfy
        (fun t -> match Loc.Located.item t with
        | NAME _ -> true
        | _ -> false)
    in
    let transform tok =
      match Tok.unpack tok with
      | NAME name -> name
      | _ -> assert false
    in
    transform <$> ident

  let capitalised_name =
    let parse () =
      let name = parse name in
      if String.is_capitalised name then
        name
      else
        parse fail
    in
    of_comp parse

  let operator s =
    lift (OPERATOR s)

  let bar = operator "|"
  let eq  = operator "="

  let parameterised_thing p =
    name <*> (many p)

  let effect_var =
    let parse () =
      match parse (optional name) with
      | None -> "£"
      | Some ident -> ident
    in
    of_comp parse

  let tyvar =
    let transform kind name =
      Make.Types.Value_type.mktyvar kind name
    in
    let eff_var =
      (transform Sk_eff) <$> (brackets capitalised_name)
    in
    let typ_var =
      (transform Sk_val) <$> capitalised_name
    in
    typ_var <|> eff_var

  let fix p =
    let rec f x = p f x in
    f

  let defer : ('a P.t Lazy.t) -> 'a P.t
    = fun p ->
    of_comp (fun () -> parse @@ Lazy.force p)

  let rec tyconstr =
    lazy(
    let constr = parameterised_thing (defer type_arg) in
    let transform (name, params) =
      Printf.printf "Constructing %s tyconstr\n" name; flush stdout;
      Make.Types.Value_type.mktyconstr name params
    in
    transform <$> constr)
  and value_type =
    lazy(
    let typ_var () =
      match parse tyvar with
      | var when var.styvar_kind = Sk_val ->
         Make.Types.Value_type.mkvar var
      | _ -> parse fail
    in
    let typ_var = of_comp typ_var in
    let constr  = Make.Types.Value_type.mkconstr <$> (defer tyconstr) in
    let alternatives = [constr; typ_var] in
    one_of alternatives)
  and type_arg =
    lazy( defer value_type) (*value_type (* <|> ability *)*)


  let data_declaration : data P.t =
    let parse () =
      let _ = parse @@ token (lift DATA) in
      let (name, params) = parse @@ parameterised_thing tyvar in
      let _ = parse (token eq) in
      let constrs =
        parse @@ separated_list ~sep:bar (defer tyconstr)
      in
      Make.Data.mkdata name params constrs
    in
    of_comp parse

  let eof = token (lift EOF)

  let program  =
    data_declaration <* eof
end
