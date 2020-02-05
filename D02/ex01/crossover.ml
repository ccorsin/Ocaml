let rec crossover l1 l2 = 
  let rec check_with_list m c = match m with
    | [] -> false
    | h::t ->
      if h = c then true
      else check_with_list t c
  in match l1 with
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

let rec print_list = function 
  [] -> print_endline ""
  | e::l -> print_string e ; print_string " " ; print_list l

let () =
  print_list (crossover ["a" ; "b" ; "c" ; "d"] ["a" ; "d" ; "x" ; "d"]);
  print_list (crossover ["a" ; "b" ; "c" ; "d"] ["w" ; "f" ; "p" ; "w"]);
  print_list (crossover [] ["a" ; "d" ; "x" ; "d"]);
  print_list (crossover ["a" ; "b" ; "c" ; "d"] []);
  print_list (crossover ["a" ; "b" ; "c" ; "d"] ["a" ; "b" ; "c" ; "d"]);