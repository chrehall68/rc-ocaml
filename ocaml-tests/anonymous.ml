let () = 
  (* sequential expressions are allowed in default values *)
  let f = fun ?thing:(b = print_endline "hi"; 42) () -> print_endline(string_of_int b) in
  f  ();
