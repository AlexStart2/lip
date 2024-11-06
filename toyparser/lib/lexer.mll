{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex = "0x" ['0'-'9''a'-'f''A'-'F']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | num { CONST (Lexing.lexeme lexbuf) }
  | hex as hexnum { CONST hexnum }
  | eof { EOF }
