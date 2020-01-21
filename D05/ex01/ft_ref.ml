type 'a ft_ref = {mutable reference : 'a}

let return a : 'a ft_ref =
  {reference = a}

let get (a:'a ft_ref) =
  a.reference

let set (a:'a ft_ref) (b:'a) =
  a.reference <- b

let bind (a:'a ft_ref) (f:('a -> 'b ft_ref)) =
  f a.reference


let () =
  let r = return (1+2) in
  print_string "This is r : ";
  print_int (get r);
  print_endline "";
  print_string "Set r at 5 : ";
  set r 5;
  print_int (get r);
  print_endline "";
  print_string "Apply square function to initial reference r to get a new reference : ";
  let f x : 'b ft_ref = {reference = x * x} in
  print_int (get (bind r f));
  print_endline "";
  print_string "And r is still at : ";
  print_int (get r);
  print_endline ""
