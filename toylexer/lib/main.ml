open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)


let rec take_first_n_elements lst n =
  match lst, n with
  | _, 0 -> []  (* If n is 0, return an empty list *)
  | [], _ -> []  (* If the list is empty, return an empty list *)
  | hd :: tl, _ -> hd :: take_first_n_elements tl (n - 1)  (* Take the head and continue *)


let freq tl =
  let a =
    List.fold_left(fun result elem -> 
      if List.exists (fun (first, _) -> first = elem) result then
        result
      else
        let count = List.length (List.find_all ((=) elem) tl) in
        List.append([elem, count]) result) [] tl
      in
      List.sort (fun (_, a) (_, b) -> compare b a) a

let frequency n tl = 
  take_first_n_elements (freq tl) n


  (* match List.hd tl with 
  | [] -> result
  | a -> let index = List.find_index(fun (_, second) -> second = a ) result in if (index = None && List.length result <n) then
    List.append([1, a]) result else *)

    (* let elem = List.hd(tl) in
    let index = List.find_index(fun (_, second) -> second = elem ) result in *)