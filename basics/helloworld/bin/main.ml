open Helloworld

let read_endline () =
  try Some(read_line())
  with End_of_file -> None

let print_int s = print_int s; print_newline ()

let split_on_word s = String.split_on_char ' ' s

let convert_to_int s = int_of_string s

(* main routine *)

let () = match read_endline () with
  Some s -> print_int (sum_of_ints(List.map convert_to_int (split_on_word s)))
| None -> print_int 0


