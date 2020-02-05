let rot42 s =
  let rotate_42 c = match c with
    | 'a'..'z' -> char_of_int (((int_of_char c) + 16 - 97) mod 26 + 97)
    | 'A'..'Z' -> char_of_int (((int_of_char c) + 16 - 65) mod 26 + 65)
    | _ -> c in
  String.map rotate_42 s

let caesar n s =
  let correct_n n =
    if n < 0 then (26 + (n mod 26))
    else n
  in
  let ft_rot_char c = match c with
    | 'a'..'z' -> char_of_int (((int_of_char c) + (correct_n n) - 97) mod 26 + 97)
    | 'A'..'Z' -> char_of_int (((int_of_char c) + (correct_n n) - 65) mod 26 + 65)
    | _ -> c
  in
  String.map ft_rot_char s

let xor key s = match key > 0 with
  | true -> 
      begin
        let do_lxor c =
          char_of_int((int_of_char c) lxor (key mod 255))
        in
        String.map do_lxor s
      end
  | false -> s

let rec ft_crypt s l = match l with
  | [] -> s
  | h::q -> ft_crypt (h s) q