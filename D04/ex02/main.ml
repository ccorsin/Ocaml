let main () =
  let all_cards = Card.all in
  let rec print_all_list l = match l with
    | [] -> print_endline ""
    | (h:Card.t)::q -> print_string (Card.toString h);
                  print_string(" - " ^ (Card.toStringVerbose h));
                  print_endline "";                   
                  print_all_list q
  in
  print_endline "All cards : ";
  print_all_list all_cards;
  print_endline "Best card of all : ";
  print_string (Card.toString (Card.best all_cards));
  print_string " - ";
  print_endline (Card.toStringVerbose (Card.best all_cards));
  print_endline "";
  print_endline "All Spades : ";
  print_all_list (Card.allSpades);
  print_endline "";
  print_endline "All Hearts : ";
  print_all_list (Card.allHearts);
  print_endline "";
  print_endline "All Diamonds : ";
  print_all_list (Card.allDiamonds);
  print_endline "";
  print_endline "All Clubs : ";
  print_all_list (Card.allClubs);
  print_endline "";
  print_endline "New card :";
  let (c1:Card.t) = Card.newCard Card.Value.As Card.Color.Heart in
  let (c2:Card.t) = Card.newCard Card.Value.T10 Card.Color.Club in
  let (c3:Card.t) = Card.newCard Card.Value.T3 Card.Color.Spade in
  print_endline ("Card 1 : " ^ Card.toStringVerbose c1 ^ " - Value : " ^ Card.Value.toStringVerbose (Card.getValue c1) ^ " - Color : " ^ Card.Color.toStringVerbose (Card.getColor c1));
  print_endline ("Is it a Club ? " ^ string_of_bool(Card.isOf c1 Card.Color.Club) ^ " | Is it a Heart ? " ^ string_of_bool(Card.isHeart c1));
  print_endline ("Card 2 : " ^ Card.toStringVerbose c2 ^ " - Value : " ^ Card.Value.toStringVerbose (Card.getValue c2) ^ " - Color : " ^ Card.Color.toStringVerbose (Card.getColor c2));
  print_endline ("Is it a Club ? " ^ string_of_bool(Card.isOf c2 Card.Color.Club) ^ " | Is it a Heart ? " ^ string_of_bool(Card.isHeart c2));
  print_endline ("Card 3 : " ^ Card.toStringVerbose c3 ^ " - Value : " ^ Card.Value.toStringVerbose (Card.getValue c3) ^ " - Color : " ^ Card.Color.toStringVerbose (Card.getColor c3));
  print_endline ("Is it a Club ? " ^ string_of_bool(Card.isOf c3 Card.Color.Club) ^ " | Is it a Heart ? " ^ string_of_bool(Card.isHeart c3));
  print_endline "";
  print_endline "Bench";
  print_endline ("Card 1 vs. Card 2 ---> " ^ " Compare : " ^ (string_of_int(Card.compare c1 c2)) ^ " | Max : " ^ (Card.toString (Card.max c1 c2)) ^ " | Min : " ^ (Card.toString (Card.min c1 c2)));
  print_endline ("Card 1 vs. Card 3 ---> " ^ " Compare : " ^ (string_of_int(Card.compare c1 c3)) ^ " | Max : " ^ (Card.toString (Card.max c1 c3)) ^ " | Min : " ^ (Card.toString (Card.min c1 c3)));
  print_endline ("Card 3 vs. Card 2 ---> " ^ " Compare : " ^ (string_of_int(Card.compare c3 c2)) ^ " | Max : " ^ (Card.toString (Card.max c3 c2)) ^ " | Min : " ^ (Card.toString (Card.min c3 c2)));
  print_endline "";
  print_endline "Best of C1 / C2 / C3";
  print_string (Card.toStringVerbose (Card.best [c1; c2; c3])); print_endline ""; print_endline "";
  print_endline "If same value C1 / C2";
  let (c1:Card.t) = Card.newCard Card.Value.T10 Card.Color.Heart in
  let (c2:Card.t) = Card.newCard Card.Value.T10 Card.Color.Club in
  print_endline ("Card 1 : " ^ Card.toStringVerbose c1);
  print_endline ("Card 2 : " ^ Card.toStringVerbose c2);
  print_endline ("C1/C2" ^ " Compare : " ^ (string_of_int(Card.compare c1 c2)) ^ " | Max : " ^ (Card.toString (Card.max c1 c2)) ^ " | Min : " ^ (Card.toString (Card.min c1 c2) ^ " | Best : " ^ Card.toStringVerbose (Card.best [c1; c2])));
  print_endline ("C2/C1" ^ " Compare : " ^ (string_of_int(Card.compare c2 c1)) ^ " | Max : " ^ (Card.toString (Card.max c2 c1)) ^ " | Min : " ^ (Card.toString (Card.min c2 c1) ^ " | Best : " ^ Card.toStringVerbose (Card.best [c2; c1])))

let () =
  main ()