let unrotate_42 c = match c with
  | 'a'..'z' -> char_of_int (((int_of_char c) + 10 - 97) mod 26 + 97)
  | 'A'..'Z' -> char_of_int (((int_of_char c) + 10 - 65) mod 26 + 65)
  | _ -> c

let unrot42 s =
  String.map unrotate_42 s

  let uncaesar s n =
    let ft_unrot_char c = match c with
    | 'a'..'z' -> char_of_int (((int_of_char c) + (26 - n) - 97) mod 26 + 97)
    | 'A'..'Z' -> char_of_int (((int_of_char c) + (26 - n) - 65) mod 26 + 65)
    | _ -> c
    in
    String.map ft_unrot_char s

let rec ft_uncrypt s l = match l with
  | [] -> s
  | h::q -> ft_uncrypt (h s) q