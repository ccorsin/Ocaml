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