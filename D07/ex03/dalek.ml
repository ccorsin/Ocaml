let rec rand_str n s = match n with
  | 0 -> s
  | 1 -> (rand_str (n - 1) s) ^ (String.make 1 (Char.chr (65 + (Random.int 26)))) 
  | _ -> (rand_str (n - 1) s) ^  (String.make 1 (Char.chr (97 + (Random.int 26))))


class dalek =
  object
    val _name : string = "Dalek" ^ rand_str 3 ""
    val _hp : int = 100
    val mutable _shield : bool = true

    method to_string = "Created object of class dalek : { name: " ^ _name ^ ", hp: " ^ string_of_int _hp ^ ", shield: " ^ string_of_bool _shield ^ " }"
    method talk = 
      let quotes = ["Explain! Explain!" ; "Exterminate! Exterminate!" ; "I obey!" ; "You are the Doctor! You are the enemy of the Daleks!"] in
      print_endline (List.nth quotes (Random.int 4))
    method exterminate (p : People.people) = p#die
    method die = print_endline "Emergency Temporal Shift!"
  end