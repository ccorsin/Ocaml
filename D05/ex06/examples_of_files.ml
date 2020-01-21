let rec create_data l (t:(float array * string)) (a:float array) i :(float array * string) = match l with
  | [] -> t
  | h::q::[] -> a.(i) <- float_of_string(h) ; create_data [] (a, q) a (i + 1)
  | h::q -> a.(i) <- float_of_string(h) ; create_data (q) t a (i + 1)

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

let translate_one_string s =
  let one_string_list = String.split_on_char ',' s in
  let a = Array.make (List.length (one_string_list ) - 1) 0. in
    create_data one_string_list (a,"") a 0

let rec translate_strings l acc = match l with
  | [] -> []
  | h::q -> translate_strings q (acc@[translate_one_string h])

let examples_of_files path : (float array * string) list =
  let all_string_list = read_lines path in
    if List.length all_string_list > 0 then
      translate_strings all_string_list []
    else
      []

let () = 
  ignore(example_of_files "ionosphere.test.csv")