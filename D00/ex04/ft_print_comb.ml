let ft_print_comb () =
  let x = 0 in
  let y = 1 in
  let z = 2 in
  let ft_print_number x y z =
    begin
    print_int x ;
    print_int y ;
    print_int z ;
    if x < 7 then
      print_string ", "
    else
      print_string "\n"
    end
  in
    let rec ft_loop x y z =
      if x <= 7 then
        begin
          ft_print_number x y z ;
          if z < 9 then
            ft_loop x y (z + 1)
          else if y < 8 then
            ft_loop x (y + 1) (y + 2)
          else if x < 7 then
            ft_loop (x + 1) (x + 2) (x + 3)
        end
    in
    ft_loop x y z

let () = ft_print_comb ()