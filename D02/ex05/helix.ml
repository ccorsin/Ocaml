type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = { 
  p : phosphate;
  d : deoxyribose;
  n : nucleobase
}

let generate_nucleotide nucleobase = match nucleobase with
  | A -> { p = "phosphate"; d = "deoxyribose"; n = A }
  | T -> { p = "phosphate"; d = "deoxyribose"; n = T }
  | C -> { p = "phosphate"; d = "deoxyribose"; n = C }
  | G -> { p = "phosphate"; d = "deoxyribose"; n = G }
  | None -> { p = "phosphate"; d = "deoxyribose"; n = None }

type helix = nucleotide list

let generate_helix n : helix =
  let rec generate_element n nucl acc = 
    if n < 1 then
      acc
    else match nucl with
    | 0 -> generate_element (n - 1) (Random.int 4) (generate_nucleotide A::acc)
    | 1 -> generate_element (n - 1) (Random.int 4) (generate_nucleotide T::acc)
    | 2 -> generate_element (n - 1) (Random.int 4) (generate_nucleotide C::acc)
    | 3 -> generate_element (n - 1) (Random.int 4) (generate_nucleotide G::acc)
    | _ -> generate_element (n - 1) (Random.int 4) (generate_nucleotide None::acc)
  in
  generate_element n (Random.int 3) []

let rec helix_to_string (h : helix) =
  let nucleobase_to_string n = match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> ""
  in
  match h with
    | [] -> ""
    | h::q -> (nucleobase_to_string h.n)^(helix_to_string q)

let complementary_helix (h : helix) : helix =
  let complementary_base b = match b.n with
    | A -> generate_nucleotide T
    | T -> generate_nucleotide A
    | C -> generate_nucleotide G
    | G -> generate_nucleotide C
    | None -> generate_nucleotide None
  in
  let rec build_helix helix acc = match helix with
    | [] -> acc
    | h::q -> build_helix q (acc@[(complementary_base h)])
  in
  build_helix h []

let () =
  Random.self_init ();
  print_string "Helix of (-1) : " ; print_string (helix_to_string (generate_helix (-1))) ; print_endline "" ;
  print_string "Helix of 0 : " ; print_string (helix_to_string (generate_helix 0)) ; print_endline "" ;
  let helix1 = generate_helix 1 in
  print_string "Helix of 1   : " ; print_string (helix_to_string helix1) ; print_endline "" ;
  print_string "Complementary: " ; print_string (helix_to_string (complementary_helix helix1)) ; print_endline "" ;
  let helix5 = generate_helix 5 in
  print_string "Helix of 5   : " ; print_string (helix_to_string helix5) ; print_endline "" ;
  print_string "Complementary: " ; print_string (helix_to_string (complementary_helix helix5)) ; print_endline "" ;
  let helix10 = generate_helix 10 in
  print_string "Helix of 10  : " ; print_string (helix_to_string helix10) ; print_endline "" ;
  print_string "Complementary: " ; print_string (helix_to_string (complementary_helix helix10)) ; print_endline "" ;