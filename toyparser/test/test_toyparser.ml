open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Some 9

(* YOUR TESTS HERE *)
let%test "test_eval_2" = parse "0xC-0xB" |> eval = Some 1
let%test "test_eval_3" = parse "(0xC-0xB)*2" |> eval = Some 2
let%test "test_eval_4" = parse "0xB*2" |> eval = Some 22
let%test "test_eval_5" = parse "0xB/2" |> eval = Some 5
let%test "test_eval_6" = parse "0xB/0" |> eval = None
