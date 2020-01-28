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

let rec insert x l arr = match l with
    | [] -> [x]
    | y::ys -> if eu_dist x arr < eu_dist y arr then x::y::ys else y::insert x ys

let rec remove_last l acc = match l with
  | h::[] -> acc
  | h::q -> remove_last l acc@[h]

let k_nn data k (r:(float array * string)) =
  let (arr, str) = r in
  let rec find_k_match d min acc =
    let (min_v, min_n, min_s) = min in
    match d with
      | [] -> (min, acc)
      | (a, s)::q ->
        begin
          let rec check_list l acc = match l with
            | [] -> acc
            | (h, s)::q -> 
              begin
                if (eu_dist h arr > eu_dist a arr) then 
                let ll = insert a l arr in
                if List.length acc = k then remove_lasr ll []
              end
          if ((eu_dist a arr) < min_n) || (min_n = (-1.)) || (List.length acc < k) then find_k_match q (a, (eu_dist a arr), s) ([(a, s)]@acc)
          else find_k_match q min acc
        end
  in
  let ((m_v, m_n, m_s), liste) = find_k_match data ([||], (-1.), "") [] in
  print_int (List.length liste) ; print_endline "" ;
  m_s


  let () =
    let data = examples_of_files "test.csv" in
    let radar1 = ([|17.2; 14.3; 11.2|], "c") in
    print_string "Radar 1 : " ; print_endline (k_nn data 4 radar1);
    (* let radar2 = ([|0.2; 4.3; 1.2|], "c") in
    print_string "Radar 2 : " ; print_endline (one_nn data radar2) *)