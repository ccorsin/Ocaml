let main () =
  Random.self_init () ;
  
  let people1 = new People.people "Celia" in
  let people2 = new People.people "Max" in
  let people3 = new People.people "Paul" in
  let people_army = new Army.army [people1] in
  people_army#add people2 ; people_army#add people3 ; people_army#print_army ;
  print_int people_army#length ; print_string " people in the army now ! " ;
  print_endline "" ; print_endline "" ;
  let dal1 = new Dalek.dalek in
  let dal2 = new Dalek.dalek in
  let dal3 = new Dalek.dalek in
  let dal_army = new Army.army [dal1] in
  dal_army#add dal2 ; dal_army#add dal3 ; dal_army#print_army ;
  print_int dal_army#length ; print_string " dalek in the army now ! " ; print_endline "" ;
  print_endline "" ; print_endline "" ;
  let doc1 = new Doctor.doctor "Doc1" 25 people1 in
  let doc2 = new Doctor.doctor "Doc2" 30 people2 in
  let doc3 = new Doctor.doctor "Doc3" 35 people3 in
  print_endline "";
  let doc_army = new Army.army [doc1] in
  doc_army#add doc2 ; doc_army#add doc3 ; doc_army#print_army ;
  print_int doc_army#length ; print_string " doctors in the army now ! " ; print_endline "" ; print_endline "" ;
  print_endline "WAR IS COMING ! " ;
  let war = new Galifrey.galifrey dal_army doc_army people_army in
  war#do_time_war


let () =
  Random.self_init ();
  main ()