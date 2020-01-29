class ['a] army (member : 'a list) =
  object
    val mutable _member : 'a list = member

    method add (a : 'a) = _member <- _member@[a]
    method remove = _member <- match _member with 
      | h::q -> q
      | _ -> []
    method length = List.length _member
    method print_army = 
      print_endline "THIS IS THE ARMY : ";
      let rec print l = match l with 
      | h::q -> print_endline (h#to_string) ; print q
      | _ -> print_endline ""
    in print _member

    method check_alive = match _member with
      | [] -> false
      | _ -> true
  
    method get_fighter i = List.nth _member i
  end