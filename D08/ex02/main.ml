let main () =
  let m1 = new Alkane.methane in
  print_endline m1#to_string ;
  let m2 = new Alkane.ethane in
  print_endline m2#to_string ;
  let m2_bis = new Alkane.ethane in
  print_endline m2#to_string ;
  let m4 = new Alkane.butane in
  print_endline m4#to_string ;
  let m8 = new Alkane.octane in
  print_endline m8#to_string ;
  print_string "ethane = ethane2 ? " ; print_endline (string_of_bool (m2#equals (m2_bis :> Molecule.molecule))) ;
	print_string "ethane = octane ? " ; print_endline (string_of_bool (m2#equals (m8 :> Molecule.molecule)))

let () =
  main ()