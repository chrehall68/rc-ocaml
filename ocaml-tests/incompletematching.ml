(* let a = Some 3;;
match a with
  Some x -> print_endline(string_of_int x) *)
let x = 33;;
match x with
| 33 -> print_endline "hi there"
| v when v lxor v == 0 -> print_endline "there"