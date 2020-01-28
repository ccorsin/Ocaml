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



(* class ['a] army (l: 'a list) = object
	val _army: 'a list = l

	method add (elem: 'a) = elem#talk ; let new_a = new army (elem :: l) in new_a#length ; new_a
	method delete = match _army with
		| elem :: tail -> elem#die ; new army tail
    | [] -> new army _army
  method length = print_int (List.length _army)
end *)