let main () =
  let rec print_list l = match l with
  | [] -> print_endline ""
  | h::q -> print_string (h ^ " - ");                  
            print_list q
  in
    Random.self_init ();
    print_endline "This is the deck : ";
    let deck = Deck.newDeck () in
      print_list (Deck.toStringListVerbose deck);
      print_list (Deck.toStringList deck);
      print_endline ("Length : " ^ string_of_int (List.length (Deck.toStringListVerbose deck)));
      let (c1, d1) = Deck.drawCard deck in
      print_endline ("Draw 1 card : " ^ (Deck.Card.toStringVerbose c1));
      let (c2, d2) = Deck.drawCard d1 in
      print_endline ("Draw another card : " ^ (Deck.Card.toStringVerbose c2))

let () =
  main ()