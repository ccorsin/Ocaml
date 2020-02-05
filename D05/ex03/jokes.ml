let read_to_string f =
  let ic = open_in f in
  let ret = really_input_string ic (in_channel_length ic) in
  close_in ic ;
  ret

let fill_base a l = 
  for i = 0 to ((List.length l) - 1) do
    a.(i) <- List.nth l i 
  done

let main () =
  try
    if Array.length Sys.argv <> 2 then
      print_endline "Usage : ./a.out [file_name]"
    else
      begin
        let file = Sys.argv.(1) in
        let jokes_string = read_to_string file in
        let jokes_list = String.split_on_char '\n' jokes_string in
          if List.length jokes_list > 0 then
            let jokes_base = Array.make (List.length jokes_list) "" in
              fill_base jokes_base jokes_list ;
              let joke = jokes_base.(Random.int (List.length jokes_list)) in
                print_endline joke
      end
  with
    | Sys_error err -> print_endline "Invalid file name" ; exit 1
    | _ -> print_endline "Error" ; exit 1

let () =
  Random.self_init ();
  main ()