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

let rec to_string l = match l with
  | [] -> ""
  | (count, num)::t -> (string_of_int count) ^ (string_of_int num) ^ (to_string t)

let rec to_list str = match (String.length str) with
    | 0 -> []
    | x -> ((int_of_char (str.[0])) - 48) :: to_list (String.sub str 1 (x - 1))

let sequence n =
  if n < 1 then
    ""
  else
    let rec build_sequence i n l =
      if i = n then
        to_string l
      else
        to_string (encode ( to_list(build_sequence (i + 1) n l)))
    in
    build_sequence 1 n "1"