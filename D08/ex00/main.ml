let main () =
  let c = new Atom.carbon in
  print_endline ("Carbone -> " ^ c#to_string);
  let o = new Atom.oxygen in
  print_endline ("Oxygene -> " ^ o#to_string);
  let h = new Atom.hydrogen in
  print_endline ("Hydrogen -> " ^ h#to_string);
  let h2 = new Atom.hydrogen in
  print_endline ("Hydrogen2 -> " ^ h2#to_string);
  print_string "H = C ? " ; print_endline (string_of_bool (h#equals o));
  print_string "H = H ? " ; print_endline (string_of_bool (h#equals h2))

let () =
  main ()