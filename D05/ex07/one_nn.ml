let eu_dist (a:float array) (b:float array) =
  let rec sum_square i acc =
    if i = (Array.length a) then
      acc
    else
      sum_square (i + 1) (acc +. ((a.(i) -. b.(i)) ** 2.))
  in
  sqrt (sum_square 0 0.)

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
  | [] -> acc
  | h::q -> translate_strings q (acc@[translate_one_string h])

let examples_of_files path : (float array * string) list =
  let all_string_list = read_lines path in
    if List.length all_string_list > 0 then
      translate_strings all_string_list []
    else
      []

let one_nn data (r:(float array * string)) =
  let (arr, str) = r in
  let rec find_match d min =
    let (min_v, min_n, min_s) = min in
    match d with
      | [] -> min
      | (a, s)::q ->
        begin
          if ((eu_dist a arr) < min_n) || (min_n = (-1.)) then find_match q (a, (eu_dist a arr), s)
          else find_match q min
        end
  in
  let (m_v, m_n, m_s) = find_match data ([||], (-1.), "") in
  m_s

  let () =
    let data = examples_of_files "test.csv" in
    let radar1 = ([|17.2; 14.3; 11.2|], "c") in
    print_endline "DATA : [| 1.0; 1.0; 1.0 |], \"g\"     [|10.3; 15.2; 12.7 |], \"f\"";
    print_string "Radar 1 [|17.2; 14.3; 11.2|] : " ; print_endline (one_nn data radar1);
    let radar2 = ([|0.2; 4.3; 1.2|], "c") in
    print_string "Radar 2 [|0.2; 4.3; 1.2|] : " ; print_endline (one_nn data radar2);
    print_endline "";
    let data = examples_of_files "test2.csv" in
    let radar1 = ([|17.2; 14.3; 11.2|], "c") in
    print_endline "DATA 2: [| 1.0; 1.0; 1.0 |], \"b\"     [|10.3; 15.2; 12.7 |], \"g\"";
    print_string "Radar 1 [|17.2; 14.3; 11.2|] : " ; print_endline (one_nn data radar1);
    let radar2 = ([|0.2; 4.3; 1.2|], "c") in
    print_string "Radar 2 [|0.2; 4.3; 1.2|] : " ; print_endline (one_nn data radar2);
    print_endline "";
    let data = examples_of_files "ionosphere.csv" in
    let radar1 = ([|1.;0.;0.66161;-1.;1.;1.;1.;-0.67321;0.80893;-0.40446;1.;-1.;1.;-0.89375;1.;0.73393;0.17589;0.70982;1.;0.78036;1.;0.85268;1.;-1.;1.;0.85357;1.;-0.08571;0.95982;-0.36250;1.;0.65268;1.;0.34732|],"b") in
    print_endline "DATA = ionosphere.csv";
    print_string "Radar l.36 : " ; print_endline (one_nn data radar1)