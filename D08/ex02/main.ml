let main () =
  let m1 = new Alkane.methane in
  print_endline m1#to_string ;
  let m2 = new Alkane.ethane in
  print_endline m2#to_string ;
  let m4 = new Alkane.butane in
  print_endline m4#to_string ;
  let m8 = new Alkane.octane in
  print_endline m8#to_string

let () =
  main ()