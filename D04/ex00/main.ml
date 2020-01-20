let main () =
  let rec loop_over_colors l = match l with
    | [] -> ()
    | h::t ->
        begin
          print_endline (Color.toString h);
          print_endline (Color.toStringVerbose h);
          loop_over_colors t
        end
  in
  loop_over_colors Color.all

let () =
  main ()