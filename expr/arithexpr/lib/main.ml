open Ast

let string_of_val = function
  | Bool true -> "true"
  | Bool false -> "false"
  | Nat n -> string_of_int n

  let rec is_nv = function
  | Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false


let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | Zero -> "0"
  | If(e0, e1, e2) ->
      "If(" ^ (string_of_expr e0) ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not(" ^ (string_of_expr e) ^ ")"
  | And(e1, e2) -> "And(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Or(e1, e2) -> "Or(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Succ(e) -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "IsZero(" ^ (string_of_expr e) ^ ")"



let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
  | If(True, e1, _) -> e1
  | If(False, _, e2) -> e2
  | If(cond, e1, e2) ->
      let cond' = trace1 cond in
      If(cond', e1, e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> Not(trace1 e)
  | And(True, e2) -> e2
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)
  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> Or(trace1 e1, e2)
  | Succ(e) -> Succ(trace1 e)
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) -> e
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies


let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


  let rec eval = function
  | True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | If(cond, then_expr, else_expr) ->
      (match eval cond with
      | Bool true -> eval then_expr
      | Bool false -> eval else_expr
      | _ -> failwith "Type error: condition of If must be a Bool")
  | Not(e) ->
      (match eval e with
      | Bool b -> Bool (not b)
      | _ -> failwith "Type error: Not requires a Bool")
  | And(e1, e2) ->
      (match (eval e1, eval e2) with
      | (Bool b1, Bool b2) -> Bool (b1 && b2)
      | _ -> failwith "Type error: And requires two Bools")
  | Or(e1, e2) ->
      (match (eval e1, eval e2) with
      | (Bool b1, Bool b2) -> Bool (b1 || b2)
      | _ -> failwith "Type error: Or requires two Bools")
  | Succ(e) ->
      (match eval e with
      | Nat n -> Nat (n + 1)
      | _ -> failwith "Type error: Succ requires a Nat")
  | Pred(e) ->
      (match eval e with
      | Nat 0 -> raise NoRuleApplies
      | Nat n -> Nat (n - 1)
      | _ -> failwith "Type error: Pred requires a Nat")
  | IsZero(e) ->
      (match eval e with
      | Nat 0 -> Bool true
      | Nat _ -> Bool false
      | _ -> failwith "Type error: IsZero requires a Nat")

