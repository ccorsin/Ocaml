let ft_print_comb2 () =
  let x = 0 in
  let y = 1 in
  let ft_print_number n =
    if n >= 0 && n < 10 then
      begin
      print_int 0 ;
      print_int n
      end
    else
      print_int n
  in
  let rec ft_iterate x y =
    if x < 99 then
      if y < 100 then
        begin
        ft_print_number x ;
        print_char ' ' ;
        ft_print_number y ;
        if x < 98 then
          print_char ',' ;
          print_char ' ' ;
          ft_iterate x (y + 1)
        end
      else
        ft_iterate (x + 1) (x + 2)   
    else
      print_char '\n'
  in
  ft_iterate x y
