let fibonacci n =
  let rec fibonacci_aux a b n =
    if n < 0 then
      (-1)
    else if n = 0 then
      a
    else
      fibonacci_aux b (a + b) (n - 1)
  in fibonacci_aux 0 1 n

let () =
  print_int (fibonacci (-42));
  print_char '\n';
  print_int (fibonacci 1);
  print_char '\n';
  print_int (fibonacci 3);
  print_char '\n';
  print_int (fibonacci 6);
  print_char '\n';