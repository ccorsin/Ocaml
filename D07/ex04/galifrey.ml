class gelifrey dalek_list doctor_list people_list =
  object
    val _dalek_army : Dalek.dalek Army.army = dalek_list
    val _doctor_army : Doctor.doctor Army.army = doctor_list
    val _people_army : People.people Army.army = people_list

    method do_time_war = match ((_dalek_army#check_alive, _doctor_army#check_alive, _people_army#check_alive)) with
        | (false, false, false) -> print_endline "Everyone is dead. END."
        | (false, false, true) -> print_endline "Only people left alive."
        | (false, true, false) -> print_endline "Only doctor left alive. " ; (_doctor_army#get_fighter 0)#travel_in_time 2020 2100
        | (true, false, false) -> print_endline "Only dalek left alive." ; (_dalek_army#get_fighter 0)#talk
        | (true, false, true) -> print_endline "Daleks and People are alive. Fight is on." ; (_dalek_army#get_fighter 0)#exterminate (_people_army#get_fighter 0) ; _people_army#remove ; (_dalek_army, _doctor_army, _people_army)#do_make_war
        | (false, true, true) -> print_endline "People and Doctor are alive. Peace." ; (_people_army#get_fighter 0)#talk ; (_doctor_army#get_fighter 0)#talk
        (* | (true, true, false) -> 
              (
                -> match Random.int 4 with
                | 0 -> (_doctor_army#get_fighter 0)#use_sonic_screwdriver ;  dalek_army#remove ; make_war (_dalek_army, _doctor_army, _people_army)
                | 1 -> (_doctor_army#get_fighter 0)#travel_in_time 10 10; _dalek_army#remove ; make_war (_dalek_army, _doctor_army, _people_army)
                | 2 -> (_dalek_army#get_fighter 0)#talk ; make_war (_dalek_army, _doctor_army, _people_army)
                | 3 -> (_doctor_army#get_fighter 0)#talk ; make_war (_dalek_army, _doctor_army, _people_army) 
              )
        | (true, true, true) ->
              (
                -> match Random.int 5 with
                  | 0 -> (_people_army#get_fighter 0)#talk ; (_doctor_army#get_fighter 0)#talk ; (_dalek_army#get_fighter 0)#talk ; (_dalek_army#get_fighter 0)#exterminate (_people_army#get_fighter 0) ; _people_army#remove 
                  | 1 -> (_doctor_army#get_fighter 0)#use_sonic_screwdriver ;  (_dalek_army#get_fighter 0)#die ; _dalek_army#delete ; make_war (_dalek_army, _doctor_army, _people_army)
                  | 2 -> (_dalek_army#get_fighter 0)#talk ; make_war (_dalek_army, _doctor_army, _people_army)
                  | 3 -> (_doctor_army#get_fighter 0)#travel_in_time 10 10; _dalek_army#delete ; make_war (_dalek_army, _doctor_army, _people_army)
                  | 4 -> (_doctor_army#get_fighter 0)#talk ; make_war (_dalek_army, _doctor_army, _people_army)
              ) *)
end