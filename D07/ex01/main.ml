let main () =
  print_endline "Class People" ;
  let people = new People.people "Celia" in
    print_endline ("To_string : " ^ (people#to_string));
    print_string "Talk      : " ; people#talk;
    print_string "Die       : " ; people#die ;
  print_endline ""; print_endline "Class Doctor" ;
  let doctor = new Doctor.doctor "Doctor" 35 people in
    print_endline ("To_string : " ^ (doctor#to_string));
    print_string "Talk      : " ; doctor#talk;
    print_string "Travel in time : " ; doctor#travel_in_time 2000 2020 ; print_endline (doctor#to_string)

let () =
  main ()