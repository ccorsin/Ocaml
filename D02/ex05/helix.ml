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
  | '_' -> { p = "phosphate"; d = "deoxyribose"; n = None }

type helix = nucleotide list

(* let generate_helix n =
  helix [{ p = "phosphate"; d = "deoxyribose"; n = A };
        { p = "phosphate"; d = "deoxyribose"; n = T };
        { p = "phosphate"; d = "deoxyribose"; n = T };
        { p = "phosphate"; d = "deoxyribose"; n = G }] *)

let rec helix_to_string h = match h.n with
| A -> 
| T -> 
| C -> 
| G -> 
| _ -> 