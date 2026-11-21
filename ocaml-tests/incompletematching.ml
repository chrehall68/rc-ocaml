(* let a = Some 3;;
match a with
  Some x -> print_endline(string_of_int x) *)
let x = 33;;
match x with
| 33 -> print_endline "hi there"
| v when v lxor v == 0 -> print_endline "there"


let x = 33.0;;
match x with
| 33.0 -> print_endline "hi there"
| v when v == x -> print_endline "there"

let x = 'h';;
match x with
| 'h' -> print_endline "hi there"
| v when v == x -> print_endline "there"
let x = "aaa";;
match x with
| "hi" -> print_endline "hi there"
| "" -> print_endline "hi there"
| "a" -> print_endline "hi there"
| v when v == x -> print_endline "there"