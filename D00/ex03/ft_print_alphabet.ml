let ft_print_alphabet () =
  let rec ft_print_letter x =
    if x <= 122 then
      begin
        print_char (char_of_int x) ;
        ft_print_letter (x + 1)
      end
    else
      print_char '\n' in
  ft_print_letter 97

let () = ft_print_alphabet ()