%token <string> IDENT
%token LET IN
%token EQ DOT COMMA
%token LPAR RPAR
%token EOF

%start <unit> prog
%%

prog:
| fundecl EOF { }

fundecl:
| IDENT EQ expr DOT { }

expr:
| LET IDENT EQ expr IN expr { }
| expr expr                 { }
| LPAR expr RPAR            { }
