let main () =
  let people = new People.people "Celia" in
  let doctor = new Doctor.doctor "Doctor" 35 people in
  let dalek = new Dalek.dalek  in
    print_endline ("People To_string : " ^ (people#to_string));
    print_endline ("Doctor To_string : " ^ (doctor#to_string));
    print_endline ("Dalek To_string : " ^ (dalek#to_string));
    print_endline "Fight !";
    people#talk ;
    doctor#talk;
    dalek#talk ;
    dalek#exterminate people ;
    doctor#use_sonic_screwdriver ;
    dalek#talk ;
    dalek#die;
    doctor#travel_in_time 2000 2020 ; print_endline (doctor#to_string)

let () =
  Random.self_init ();
  main ()