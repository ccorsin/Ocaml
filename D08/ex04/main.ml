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

let main () =
  try
    let m1 = new Alkane.methane in
    print_endline m1#to_string ;
    let m12 = new Alkane.methane in
    print_endline m12#to_string ;
    let m2 = new Alkane.ethane in
    print_endline m2#to_string ;
    print_endline "Let's react ! ";
    print_string "Alkanes list : " ;
    let r1 = new Alkane_combustion.alkane_combustion [m1 ; m2 ; m12] in
    print_list (r1#count_alkanes) ; print_endline "" ;
    print_string "Contains -> "; print_int (r1#count_C) ; print_string " Carbones & " ; print_int (r1#count_H) ; print_endline " Hydrogen."; print_endline "";
    print_endline ""; 
    print_string "Recation équilibrée ? " ; print_string (string_of_bool r1#is_balanced) ; print_endline "";
    print_endline "Call for balance ==> " ;
    let r2 = r1#balance in
    print_string "Recation équilibrée ? " ; print_string (string_of_bool r2#is_balanced) ; print_endline "" ;
    print_string "Reactifs : " ; print_list_short (r2#get_start); print_endline "";
    print_string "Produits : " ; print_list_short (r2#get_result); print_endline "";
    print_string "Reaction : " ; print_list_short (r2#get_start) ; print_string "= " ; print_list_short (r2#get_result) ; print_endline "";
    print_endline "" ; print_endline "With previous unbalanced reaction : " ;
    print_string "Reactifs : " ; print_list_short (r1#get_start); print_endline "";
    print_string "Produits : " ; print_list_short (r1#get_result); print_endline ""
  with Alkane_combustion.Unbalanced s -> print_endline s

let () =
  main ()