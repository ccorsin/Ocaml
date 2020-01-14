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

  