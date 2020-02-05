type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | None

type nucleotide = { 
  p : phosphate;
  d : deoxyribose;
  n : nucleobase
}

let generate_nucleotide nucleobase = match nucleobase with
  | 'A' -> { p = "phosphate"; d = "deoxyribose"; n = A }
  | 'T' -> { p = "phosphate"; d = "deoxyribose"; n = T }
  | 'C' -> { p = "phosphate"; d = "deoxyribose"; n = C }
  | 'G' -> { p = "phosphate"; d = "deoxyribose"; n = G }
  | _ -> { p = "phosphate"; d = "deoxyribose"; n = None }

let () =
  let print_nucleobase n = match n with
    | A -> "A" 
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"
  in
  print_string "For 'A' : ";
  let nuclA = generate_nucleotide 'A' in
  print_string " p = " ; print_string nuclA.p ; print_string " d = " ; print_string nuclA.d ; print_string " n = " ; print_string (print_nucleobase nuclA.n) ; print_endline "" ;
  print_string "For 'T' : ";
  let nuclT = generate_nucleotide 'T' in
  print_string " p = " ; print_string nuclT.p ; print_string " d = " ; print_string nuclT.d ; print_string " n = " ; print_string (print_nucleobase nuclT.n) ; print_endline "" ;
  print_string "For 'C' : ";
  let nuclC = generate_nucleotide 'C' in
  print_string " p = " ; print_string nuclC.p ; print_string " d = " ; print_string nuclC.d ; print_string " n = " ; print_string (print_nucleobase nuclC.n) ; print_endline "" ;
  print_string "For 'G' : ";
  let nuclG = generate_nucleotide 'G' in
  print_string " p = " ; print_string nuclG.p ; print_string " d = " ; print_string nuclG.d ; print_string " n = " ; print_string (print_nucleobase nuclG.n) ; print_endline "" ;
  print_string "For 'P' : ";
  let nuclNone = generate_nucleotide 'P' in
  print_string " p = " ; print_string nuclNone.p ; print_string " d = " ; print_string nuclNone.d ; print_string " n = " ; print_string (print_nucleobase nuclNone.n) ; print_endline "" ;