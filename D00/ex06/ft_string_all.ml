let ft_string_all f s =
  let len = 0 in
    let rec ft_loop f s len =
      if f (String.get s len) = false then
        false
      else if len < String.length s - 1 then
        ft_loop f s (len + 1)
      else
        true
    in
    ft_loop f s len

let () =
  let is_digit c = c >= '0' && c <= '9' in
  if ft_string_all is_digit "0123456789" then
    print_endline "true"
  else print_endline "false";
  if ft_string_all is_digit "012EAS67B9" then
    print_endline "true"
  else print_endline "false";
  if ft_string_all is_digit "a123456789" then
    print_endline "true"
  else print_endline "false"