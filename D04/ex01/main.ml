let main () =
  let rec loop_over_values l = match l with
    | [] -> ()
    | h::t ->
          print_string "Int : ";
          print_int (Value.toInt h);
          print_endline "";
          print_string "Strings : ";
          print_string (Value.toString h);
          print_string (" - ");
          print_endline (Value.toStringVerbose h);
          print_string "Next : ";
          if Value.toInt h <> 13 then
            print_endline (Value.toString (Value.next h))
          else print_endline ("Invalid arg");
          print_string "Previous : ";
          if Value.toInt h <> 1 then
           print_endline (Value.toString (Value.previous h))
          else print_endline ("Invalid arg");
          print_endline "";
          loop_over_values t
  in
  loop_over_values Value.all

let () =
  main ()