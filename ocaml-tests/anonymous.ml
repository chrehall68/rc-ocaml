let () = 
  (* sequential expressions are allowed in default values *)
  let f = fun ?thing:(b = print_endline "hi"; 42) () -> print_endline(Int.to_string b) in
  f  ();
