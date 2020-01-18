let rec ft_power x y =
  if y = 0 then
    1
  else
    if x = 0 then
      0
    else
      if y = 1 then
        x
      else
        x * ft_power x (y - 1)

let () =
  print_int (ft_power 2 4);
  print_endline "";
  print_int (ft_power 3 0);
  print_endline "";  
  print_int (ft_power 0 5);
  print_endline "";
  print_int (ft_power 5 1);
  print_endline "";
  print_int (ft_power 1 5);
  print_endline "";