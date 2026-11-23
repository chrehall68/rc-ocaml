let a = true;;
match a with
  true -> () |
  _ when true -> ()
;;

let x = 33;;
match x with
| 0 -> print_endline "hi there"
| v when v lxor v == 0 -> print_endline "there" 

 
let x = 33.0;;
match x with
| 33.0 -> print_endline "hi there"
| v when v == x -> print_endline "there";;

let x = 'h';;
match x with
| 'h' -> print_endline "hi there"
| v when v == x -> print_endline "there";;
let x = "aaa";;
match x with
| "hi" -> print_endline "hi there"
| "" -> print_endline "hi there"
| "a" -> print_endline "hi there"
| v when v == x -> print_endline "there";;

let f ~a ~b ?(c=3) x ~d y = a+b+c+d+x+y;;

let x = f 4 ~a:3 ~c:5