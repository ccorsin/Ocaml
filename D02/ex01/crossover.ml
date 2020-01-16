let rec check_with_list l2 c = match l2 with
  | [] -> false
  | h::t ->
    if h = c then true
    else check_with_list t c

let rec crossover l1 l2 = match l1 with
  | [] -> []
  | h::t ->
    begin
      if check_with_list l2 h then
        if check_with_list (crossover t l2) h = false then
          [h]@(crossover t l2)
        else
          crossover t l2
      else
        crossover t l2
    end