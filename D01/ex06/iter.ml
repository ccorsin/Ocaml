let rec iter (f : int -> int) x n =
  if n = 0 then
    x
  else
    iter f (f x) (n - 1)

let () =
  print_int (iter (fun x -> x * x) 2 4);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 4);
  print_char '\n'