let main () =
  print_endline "Tests of Cipher & Uncipher ";
  print_string "-> rot42 of abcdefghijklmnopqrstuvwxyz : ";
  let s = Cipher.rot42 "abcdefghijklmnopqrstuvwxyz" in print_endline s;
  print_string "-> unrot42 of qrstuvwxyzabcdefghijklmnop : ";
  let s = Uncipher.unrot42 "qrstuvwxyzabcdefghijklmnop" in print_endline s; print_endline "";
  print_string "-> rot42 of 1ABC478987abc : ";
  let s = Cipher.rot42 "1ABC478987abc" in print_endline s;
  print_string "-> unrot42 of 1QRS478987qrs : ";
  let s = Uncipher.unrot42 "1QRS478987qrs" in print_endline s; print_endline "";
  print_string "-> caesar of abcdefghijklmnopqrstuvwxyz | 42 : ";
  let s = Cipher.caesar 42  "abcdefghijklmnopqrstuvwxyz" in print_endline s;
  print_string "-> uncaesar of qrstuvwxyzabcdefghijklmnop | 42 : ";
  let s = Uncipher.uncaesar 42 "qrstuvwxyzabcdefghijklmnop" in print_endline s; print_endline "";
  print_string "-> caesar of abcdefghijklmnopqrstuvwxyz | 0 : ";
  let s = Cipher.caesar 0 "abcdefghijklmnopqrstuvwxyz" in print_endline s;
  print_string "-> uncaesar of abcdefghijklmnopqrstuvwxyz | 0 : ";
  let s = Uncipher.uncaesar 0 "abcdefghijklmnopqrstuvwxyz" in print_endline s;  print_endline "";
  print_string "-> caesar of abcdefghijklmnopqrstuvwxyz | 1 : ";
  let s = Cipher.caesar 1 "abcdefghijklmnopqrstuvwxyz" in print_endline s;
  print_string "-> uncaesar of bcdefghijklmnopqrstuvwxyza | 1 : ";
  let s = Uncipher.uncaesar 1 "bcdefghijklmnopqrstuvwxyza" in print_endline s; print_endline "";
  print_string "-> caesar of HelloWorld42 | 7 : ";
  let s = Cipher.caesar 7 "HelloWorld42" in print_endline s;
  print_string "-> uncaesar of OlssvDvysk42 | 7 : ";
  let s = Uncipher.uncaesar 7 "OlssvDvysk42" in print_endline s; print_endline "";
  print_string "-> xor of HelloWorld42 | 12 : ";
  let s = Cipher.xor 12 "HelloWorld42" in print_endline s; 
  print_string "-> xor of Di``c[c~`h8> | 12 : ";
  let s = Cipher.xor 12 "Di``c[c~`h8>" in print_endline s;  print_endline "";
  print_string "-> ft_crypt  : HelloWorld42 -> (Cipher.caesar 12); (Cipher.xor 5); (Cipher.rot42) : ";
  let s1 = Cipher.ft_crypt "HelloWorld42" [(Cipher.caesar 12); (Cipher.xor 5); Cipher.rot42] in print_endline s1; print_endline "";
  print_string "-> ft_uncrypt : "; print_string s1; print_string " -> (Uncipher.unrot42) ; (Cipher.xor 5); (Uncipher.uncaesar 12) : ";
  let s2 = Uncipher.ft_uncrypt s1 [Uncipher.unrot42; (Cipher.xor 5); (Uncipher.uncaesar 12)] in print_endline s2

let () =
  main ()