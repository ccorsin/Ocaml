let ft_is_palindrome s =
  let i = 0 in
  let j = String.length s - 1 in
  let rec ft_loop s i j =
    if i = j || j < 0 then
      true
    else
      if String.get s i = String.get s j then
        ft_loop s (i + 1) (j - 1)
      else
        false
    in
    ft_loop s i j