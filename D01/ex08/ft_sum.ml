let rec ft_sum (f : int -> float) (i : int) (n : int) =
    if n < i then
      nan
    else if n = i then
      f i
    else
      f i +. ft_sum f (i + 1) n

let () =
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  print_char '\n';
  print_float (ft_sum (fun i -> float_of_int (i)) 1 10);
  print_char '\n';
  print_float (ft_sum (fun i -> float_of_int (i)) 0 10);
  print_char '\n';
  print_float (ft_sum (fun i -> float_of_int (i)) 10 2);
  print_char '\n'