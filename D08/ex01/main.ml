let main () =
  print_endline "Molecules :" ;
  let water = new Molecule.water in
  print_endline ("Water molecule : " ^ water#to_string);
  let carbon_d = new Molecule.carbon_dioxyde in
  print_endline ("Carbon dioxyde molecule : " ^ carbon_d#to_string);
  let no = new Molecule.nitrogen_monoxide in
  print_endline ("Nitric oxyde : " ^ no#to_string);
  let caco3 = new Molecule.calcium_carbonate in
  print_endline ("Calcium carbonate : " ^ caco3#to_string);
  let al2o3 = new Molecule.aluminium_oxide in
  print_endline ("Alumine : " ^ al2o3#to_string);
  let tnt = new Molecule.trinitrotoluene in
  print_endline ("TNT : " ^ tnt#to_string);
  let tnt2 = new Molecule.trinitrotoluene in
  print_endline ("TNT2 : " ^ tnt2#to_string);
  print_string "TNT = water ? " ; print_endline (string_of_bool (tnt#equals water));
  print_string "TNT = TNT ? " ; print_endline (string_of_bool (tnt#equals tnt2))

let () =
  main ()