let ft_rot_n n str =
  let ft_rot_char c =
    if (int_of_char c > 96) && (int_of_char c < 123) then
      char_of_int (((int_of_char c) + n - 97) mod 26 + 97)
    else if (int_of_char c > 64) && (int_of_char c < 91) then
      char_of_int (((int_of_char c) + n - 65) mod 26 + 65)
    else
      c
  in
  String.map ft_rot_char str

  let () =
    print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
    print_endline (ft_rot_n 42 "0123456789");
    print_endline (ft_rot_n 2 "OI2EAS67B9");
    print_endline (ft_rot_n 0 "Damned !");
    print_endline (ft_rot_n 42 "");
    print_endline (ft_rot_n 1 "NBzlk qnbjr !");