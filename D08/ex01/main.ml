let main () =
  let c = new Atom.carbon in
  print_endline ("Carbone -> " ^ c#to_string);
  let o = new Atom.oxygen in
  print_endline ("Oxygene -> " ^ o#to_string);
  let h = new Atom.hydrogen in
  print_endline ("Hydrogen -> " ^ h#to_string); print_endline "" ; print_endline "";
  print_endline "Molecules :" ;
  let water = new Molecule.water in
  print_endline ("Water molecule : " ^ water#to_string);
  let carbon_d = new Molecule.carbon_dioxyde in
  print_endline ("Carbon dioxyde molecule : " ^ carbon_d#to_string);
  let no = new Molecule.nitrogen_monoxide in
  print_endline ("Carbon dioxyde molecule : " ^ no#to_string);
  let caco3 = new Molecule.calcium_carbonate in
  print_endline ("Calcium carbonate : " ^ caco3#to_string);
  let al2o3 = new Molecule.aluminium_oxide in
  print_endline ("Alumine : " ^ al2o3#to_string);
  let tnt = new Molecule.trinitrotoluene in
  print_endline ("TNT : " ^ tnt#to_string)

let () =
  main ()