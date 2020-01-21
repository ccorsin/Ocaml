let eu_dist (a:float array) (b:float array) =
  let rec sum_square a b i acc =
    if i = Array.length a then
      acc
    else
      acc + ((a.(i) - b.(i)) ** 2);
      sum_square a b (i + 1) acc
  in
  sum_square a b 1 0