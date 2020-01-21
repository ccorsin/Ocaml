let read_lines f : string list =
  try
    let ic = open_in f in
    let try_read () =
      try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
      | Some s -> loop (s :: acc)
      | None -> close_in ic; List.rev acc in
    loop []
  with
  | Sys_error err -> print_endline "Invalid file name" ; exit 1

let rec fill_base a l i = match l with
    | [] -> ()
    | h::q -> a.(i) <- h ; fill_base a q (i + 1)

let main () =
  if Array.length Sys.argv = 2 then
    let file = Sys.argv.(1) in
    let jokes_list = read_lines file in
      if List.length jokes_list > 0 then
        let jokes_base = Array.make (List.length jokes_list) "" in
          fill_base jokes_base jokes_list 0;
          let joke = jokes_base.(Random.int (List.length jokes_list)) in
            print_endline joke

let () =
  Random.self_init ();
  main ()