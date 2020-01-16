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
