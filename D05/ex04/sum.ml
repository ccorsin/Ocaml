let sum (x:float) (y:float) :float =
  x +. y

let () =
  print_string "2. + 10. = ";
  print_float (sum 2. 10.);
  print_endline "";
  print_string "2.5 + 2.5 = ";
  print_float (sum 2.5 2.5);
  print_endline "";
  print_string "(-2.) + 7. = ";
  print_float (sum (-2.) 7.);
  print_endline "";
  print_string "10.9 + 9.1 = ";
  print_float (sum 10.9 9.1);
  print_endline "";
  print_string "(-10.) + 20. = ";
  print_float (sum (-10.) 20.);
  print_endline "";