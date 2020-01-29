let main () =
  let c = new Atom.carbon in
  print_endline ("Carbone -> " ^ c#to_string);
  let o = new Atom.oxygen in
  print_endline ("Oxygene -> " ^ o#to_string);
  let h = new Atom.hydrogen in
  print_endline ("Hydrogen -> " ^ h#to_string)

let () =
  main ()