let () = 
  let x = None in
  let res = match print_endline("in match"); x with 
    | Some v when print_endline("first"); v mod 3 == 1 -> print_endline("yessums"); 1
    | Some v when print_endline("second"); v mod 3 == 0 -> print_endline("noums"); 2
    | Some v -> print_endline("catchall some"); 3
    | None -> 5; 8
  in
  print_endline(Int.to_string res)
