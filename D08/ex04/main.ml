let rec print_list l = match l with
  | [] -> print_string ""
  | (m, i)::q -> (print_int i); print_string (" , " ^ m#to_string^" ; ") ; print_list q

let rec print_list_short l = match l with
  | [] -> print_string ""
  | (m, i)::[] -> 
    if i <> 1 then print_int i
    else () ;
    print_string (" " ^ (m#formula) ^"  ") ; print_list_short []
  | (m, i)::q -> 
    if i <> 1 then print_int i
    else ();
    print_string (" " ^ (m#formula) ^" + ") ; print_list_short q

let rec print_atom_list l = match l with
  | [] -> print_endline ""
  | (i, a)::q -> (print_int i); print_string (" , " ^ a#to_string^" ; ") ; print_atom_list q

let main () =
  let m1 = new Alkane.methane in
  print_endline m1#to_string ;
  let m12 = new Alkane.methane in
  print_endline m12#to_string ;
  let m2 = new Alkane.ethane in
  print_endline m2#to_string ;
  let m4 = new Alkane.butane in
  print_endline m4#to_string ;
  let m8 = new Alkane.octane in
  print_endline m8#to_string ;
  print_endline "Let's react ! ";
  print_string "Alkanes list : " ;
  let r1 = new Alkane_combustion.alkane_combustion [m1 ; m2 ; m12] in
  print_list (r1#count_alkanes) ; print_endline "" ;
  print_int (r1#count_C) ; print_string " Carbones & " ; print_int (r1#count_H) ; print_endline " Hydrogen."; print_endline "";
  print_string "Reactifs : " ; print_list_short (r1#get_start); print_endline "";
  print_string "Produits : " ; print_list_short (r1#get_result); print_endline "";
  print_string "Raction : " ; print_list_short (r1#get_start) ; print_string "= " ; print_list_short (r1#get_result) ; print_endline "";
  print_atom_list (r1#atoms);
  print_endline (string_of_bool r1#is_balanced) 

let () =
  main ()