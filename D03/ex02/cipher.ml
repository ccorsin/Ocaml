let rotate c n = match c with
  | 'a'..'z' -> char_of_int (((int_of_char c) + n - 97) mod 26 + 97)
  | 'A'..'Z' -> char_of_int (((int_of_char c) + n - 65) mod 26 + 65)
  | _ -> c

let rot42 s =
  String.map rotate s