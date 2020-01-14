let ft_rot_n x s =
  let ft_rot_char c =
    if (int_of_char c > 96) && (int_of_char c < 123) then
      char_of_int (((int_of_char c) + x - 97) mod 26 + 97)
    else if (int_of_char c > 64) && (int_of_char c < 91) then
      char_of_int (((int_of_char c) + x - 65) mod 26 + 65)
    else
      c
  in
  String.map ft_rot_char s