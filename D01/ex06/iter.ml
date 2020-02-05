let rec iter (f : int -> int) x n =
  if n = 0 then
    x
  else if n < 0 then
    (-1)
  else
    iter f (f x) (n - 1)

let () =
  print_int (iter (fun x -> x * x) 2 4);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 4);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 (-42));
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 0);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 1);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 2);
  print_char '\n'