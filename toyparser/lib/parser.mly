%{
open Ast
%}

%token <string> CONST
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left PLUS
%left MINUS
%left TIMES
%left DIV
%left UMINUS

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;



expr:
  | n = CONST {
      if String.length n > 1 && (String.sub n 0 2 = "0x" || String.sub n 0 2 = "0X")
      then Const(int_of_string n)  (* Parses hex string to int *)
      else Const(int_of_string n)  (* Parses decimal string to int *)
    }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; TIMES; e2 = expr { Mul(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | MINUS e=expr %prec UMINUS { Mul(Const(-1), e) }
  | LPAREN; e=expr; RPAREN {e}
;

