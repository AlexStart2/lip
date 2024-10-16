(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
(* Hint: you can use the following function to convert a string into a list of char:
```ocaml
``` *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let toklist_of_string s = explode s |> List.map (fun c -> match c with
    | 'A' -> A
    | 'B' -> B
    | '=' -> X
    | _ -> failwith "invalid token"
  )

(* val string_of_tok : token -> string *)
(* string_of_tok t transforms the token t into a string *)

(* val string_of_tok : token -> string *)
(* string_of_tok t transforms the token t into a string *)

(* val string_of_toklist : token list -> string *)
(* string_of_toklist l transforms the list of tokens l into a string *)
 
(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = failwith "TODO"

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = failwith "TODO"

(* val string_of_winner : token -> string *)
let string_of_winner w = failwith "TODO"
