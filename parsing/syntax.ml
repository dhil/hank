type name = string

type tyvar =
  | Eff of name
  | Reg of name

type tyvars = tyvar list

type decl =
  | Data of name * tyvars * constr list
and constr =
  | Constr of name * name list
and expr =
  | Name of name
