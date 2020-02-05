let encode l =
  let rec encode_list l acc n = match l with
    | [] -> acc
    | h::next::t ->
      begin
        if h = next then
          encode_list (next::t) acc (n + 1)
        else
          encode_list (next::t) (acc@[(n + 1, h)]) 0
      end
    | h::t -> encode_list [] (acc@[(n + 1, h)]) 0
  in
  encode_list l [] 0

let rec print_list li = match li with 
  | [] -> print_endline ""
  | (count, c)::l -> print_int count ; print_string c ; print_list l

let () =
  print_string "aaabb : ";
  print_list (encode ["a" ; "a" ; "a" ; "b" ; "b"]);
  print_endline "";
  print_string "123 : ";
  print_list (encode ["1" ; "2" ; "3"]);
  print_endline "";
  print_string "[] : ";
  print_list (encode []);
  print_endline "";
  print_string "qqrrqqq : ";
  print_list (encode ["q" ; "q" ; "r" ; "r" ; "q" ; "q" ; "q"]);
  print_endline "";
  print_string "??? : ";
  print_list (encode ["?" ; "?" ; "?"]);
  print_endline "";
