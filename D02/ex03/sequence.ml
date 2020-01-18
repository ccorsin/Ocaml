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

let encode l =
  encode_list l [] 0

let rec from_tuples_to_list l = match l with
  | [] -> []
  | (count, num)::q -> (string_of_int count)::(num)::(from_tuples_to_list q)
  
let rec from_list_to_string l s = match l with
  | [] -> s
  | h::q -> from_list_to_string q (s ^ h)

let sequence n =
  if n < 1 then
    ""
  else
    let rec build_sequence i n l =
      if i = n then
        from_list_to_string l ""
      else
        build_sequence (i + 1) n (from_tuples_to_list (encode l))
    in
    build_sequence 1 n ["1"]

let () =
  print_endline (sequence 0);
  print_endline (sequence (-10));
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4);
  print_endline (sequence 5);
  print_endline (sequence 6);
  print_endline (sequence 7);
  print_endline (sequence 8);