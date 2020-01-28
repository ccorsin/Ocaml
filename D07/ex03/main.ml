let main () =
  let people1 = new People.people "Celia" in
  let people2 = new People.people "Max" in
  let people3 = new People.people "Paul" in
  print_endline "";
  print_string "Build People Army with : " ; print_string (people1#to_string) ; print_endline "";
  let people_army = new Army.army [people1] in
  people_army#print_army ;
  print_int people_army#length ; print_string " people in the army now ! " ; print_endline "" ;
  print_endline "" ; print_string "Add Max -> " ; people_army#add people2 ;
  people_army#print_army ;
  print_int people_army#length ; print_string " people in the army now ! " ; print_endline "" ;
  print_endline "" ; print_string "Add Paul -> " ; people_army#add people3 ;
  people_army#print_army ;
  print_int people_army#length ; print_string " people in the army now ! " ; print_endline "" ;
  print_endline "" ; print_endline "" ; print_endline "Remove one people : " ; people_army#remove ;
  print_int people_army#length ; print_string " people in the army now ! " ; print_endline "" ;
  people_army#print_army ;
  print_endline "" ; print_endline "And another... " ; people_army#remove ;
  people_army#print_army ;
  print_int people_army#length ; print_string " people in the army now ! " ; print_endline "" ;
  print_endline "" ; print_endline "And the last. " ; people_army#remove ;
  people_army#print_army ;
  print_int people_army#length ; print_string " people in the army now ! " ; print_endline "" ;
  print_endline "" ; print_endline "" ;
  let dal1 = new Dalek.dalek in
  let dal2 = new Dalek.dalek in
  let dal3 = new Dalek.dalek in
  print_endline "";
  print_string "Build Dalek Army with : " ; print_string (dal1#to_string) ; print_endline "";
  let dal_army = new Army.army [dal1] in
  print_int dal_army#length ; print_string " dalek in the army now ! " ; print_endline "" ;
  print_string "Add dal2 -> " ; dal_army#add dal2 ;
  print_int dal_army#length ; print_string " dalek in the army now ! " ; print_endline "" ;
  print_string "Add dal3 -> " ; dal_army#add dal3 ;
  print_int dal_army#length ; print_string " dalek in the army now ! " ; print_endline "" ;
  print_endline "" ; print_endline "Remove one dalek : " ; dal_army#remove ;
  print_int dal_army#length ; print_string " dalek in the army now ! " ; print_endline "" ;
  print_endline "And another... " ; dal_army#remove ;
  print_int dal_army#length ; print_string " dalek in the army now ! " ; print_endline "" ;
  print_endline "And the last. " ; dal_army#remove ;
  print_int dal_army#length ; print_string " dalek in the army now ! " ; print_endline "";
  print_endline "" ; print_endline "" ;
  let doc1 = new Doctor.doctor "Doc1" 25 people1 in
  let doc2 = new Doctor.doctor "Doc2" 30 people2 in
  let doc3 = new Doctor.doctor "Doc3" 35 people3 in
  print_endline "";
  print_string "Build Doctor Army with : " ; print_string (doc1#to_string) ; print_endline "";
  let doc_army = new Army.army [doc1] in
  print_int doc_army#length ; print_string " doctors in the army now ! " ; print_endline "" ;
  print_string "Add doc2 -> " ; doc_army#add doc2 ;
  print_int doc_army#length ; print_string " doctors in the army now ! " ; print_endline "" ;
  print_string "Add doc3 -> " ; doc_army#add doc3 ;
  print_int doc_army#length ; print_string " doctors in the army now ! " ; print_endline "" ;
  print_endline "" ; print_endline "Remove one doctor : " ; doc_army#remove ;
  print_int doc_army#length ; print_string " doc in the army now ! " ; print_endline "" ;
  print_endline "And another... " ; doc_army#remove ;
  print_int doc_army#length ; print_string " doc in the army now ! " ; print_endline "" ;
  print_endline "And the last. " ; doc_army#remove ;
  print_int doc_army#length ; print_string " doc in the army now ! " ; print_endline ""


let () =
  Random.self_init ();
  main ()