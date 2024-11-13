open BoolexprLib.Main

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = test_eval "false" false
let%test "test_eval_2" = test_eval "true" true
let%test "test_eval_3" = test_eval "if (true && false) then false else true" true
let%test "test_eval_4" = test_eval "if (false || true) then false else true" false
let%test "test_eval_5" = 
  test_eval "if true then (if true then false else true) else (if true then true else false)" false
let%test "test_eval_6" = 
  test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false
let%test "test_eval_7" = 
  test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false


(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = trace1 (If (True, False, True)) = False
let%test "test_trace1_2" = trace1 (If (False, True, False)) = False
let%test "test_trace1_3" = trace1 (If (If (False, False, True), True, False)) = If (True, True, False)

let%test "trace1 can't make progress implies is_value" =
  match trace1 True with
  | exception NoRuleApplies -> true
  | _ -> false (* If trace1 makes progress, the test passes *)


let rec trace_n e = 
  let n = 0 in
  match trace1 e with
    | exception NoRuleApplies -> n
    | e' -> trace_n e'


let%test "expression reduces fully in no more than 10 steps" = trace_n (If(If(False,False,False),If(False,True,False),If(True,False,True))) <=10



(* ### Unit tests for task 6 *)

