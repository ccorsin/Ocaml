let ft_print_rev s =
  let len = (String.length s - 1) in
    let rec ft_print_char (len) =
      if len >= 0 then 
        begin
        print_char (String.get s len);
        if len > 0 then
          ft_print_char (len - 1)
        else
          print_char '\n'
        end
      else
      print_char '\n'
      in
      ft_print_char len

let () =
  ft_print_rev "Hello world !";
  ft_print_rev "";
  ft_print_rev "42";