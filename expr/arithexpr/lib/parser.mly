%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token NOT
%token SUCC
%token PRED
%token ISZERO
%token ZERO
%token AND
%token OR
%token EOF

%start <expr> prog

%left OR
%left AND
%nonassoc NOT ISZERO SUCC PRED

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | ZERO { Zero }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | NOT; e = expr; { Not(e) }
  | ISZERO; e = expr; { IsZero(e) }
  | SUCC; e = expr; { Succ(e) }
  | PRED; e = expr; { Pred(e) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
;
