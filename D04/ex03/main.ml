let main () =
  let rec print_list l = match l with
  | [] -> print_endline ""
  | h::q -> print_string (h ^ "  ");                  
            print_list q
  in
    Random.self_init ();
    print_endline "This is the deck : "; print_endline "";
    let deck = Deck.newDeck () in
      print_list (Deck.toStringListVerbose deck); print_endline "";
      print_list (Deck.toStringList deck);
      print_endline ("Length : " ^ string_of_int (List.length (Deck.toStringListVerbose deck))); print_endline "";
      let (c1, d1) = Deck.drawCard deck in
      print_endline ("Draw 1 card : " ^ (Deck.Card.toStringVerbose c1));
      print_endline ("Length of the deck now : " ^ string_of_int (List.length (Deck.toStringListVerbose d1)));
      let (c2, d2) = Deck.drawCard d1 in
      print_endline ("Draw another card : " ^ (Deck.Card.toStringVerbose c2));
      print_endline ("Length of the deck now : " ^ string_of_int (List.length (Deck.toStringListVerbose d2)));
      print_list (Deck.toStringListVerbose d2);
      print_string "Draw card on empty deck : "; print_endline "";
      let rec draw_all_cards i d =
        if i < 0 then ""
        else
          let (c_new, d_new) = Deck.drawCard d in
            draw_all_cards (i - 1) d_new
      in
      print_string (draw_all_cards 55 deck);
      print_endline ""

let () =
  main ()