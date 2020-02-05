type phosphate = string

type deoxyribose = string

type nucleobase = A | T | C | G | U | None

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
  | U -> { p = "phosphate"; d = "deoxyribose"; n = U }
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
    | _ -> ""
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
    | _ -> generate_nucleotide None
  in
  let rec build_helix helix acc = match helix with
    | [] -> acc
    | h::q -> build_helix q (acc@[(complementary_base h)])
  in
  build_helix h []

type rna = nucleobase list

let generate_rna (h : helix) : rna =
  let complementary_base_rna b = match b.n with
    | A -> U
    | T -> A
    | C -> G
    | G -> C
    | _ -> None
  in
  let rec build_helix_rna helix acc = match helix with
    | [] -> acc
    | h::q -> build_helix_rna q (acc@[(complementary_base_rna h)])
  in
  build_helix_rna h []

let rev l =
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] l

let generate_bases_triplets (rna:rna) =
  let rec generate_triplets r acc = match r with
  | [] -> acc
  | a::b::c::d -> generate_triplets d [(a, b, c)]@acc
  | _ -> acc
  in
  rev (generate_triplets rna [])

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val

type protein = aminoacid list

let string_of_protein (p:protein) =
  let rec create_string l s = match l with
    | [] -> s
    | Stop::q -> s^"End of translation"
    | Ala::q -> create_string q (s^"Alanine ")
    | Arg::q -> create_string q (s^"Arginine ")
    | Asn::q -> create_string q (s^"Asparagine ")
    | Asp::q -> create_string q (s^"Aspartique ")
    | Cys::q -> create_string q (s^"Cysteine ")
    | Gln::q -> create_string q (s^"Glutamine ")
    | Glu::q -> create_string q (s^"Glutamique ")
    | Gly::q -> create_string q (s^"Glycine ")
    | His::q -> create_string q (s^"Histidine ")
    | Ile::q -> create_string q (s^"Isoleucine ")
    | Leu::q -> create_string q (s^"Leucine ")
    | Lys::q -> create_string q (s^"Lysine ")
    | Met::q -> create_string q (s^"Methionine ")
    | Phe::q -> create_string q (s^"Phenylalanine ")
    | Pro::q -> create_string q (s^"Proline ")
    | Ser::q -> create_string q (s^"Serine ")
    | Thr::q -> create_string q (s^"Threonine ")
    | Trp::q -> create_string q (s^"Tryptophane ")
    | Tyr::q -> create_string q (s^"Tyrosine ")
    | Val::q -> create_string q (s^"Valine ")
  in
  create_string p ""

let decode_arn (rna:rna) :protein =
  let triplets = generate_bases_triplets rna in
  let rec decode l acc = match l with
    | [] -> acc
    | (U,A,A)::(U,A,G)::(U,G,A)::q -> [Stop]@acc
    | (G,C,A)::(G,C,C)::(G,C,G)::(G,C,U)::q -> decode q [Ala]@acc
    | (A,G,A)::(A,G,G)::(C,G,A)::(C,G,C)::(C,G,G)::(C,G,U)::q -> decode q [Arg]@acc
    | (A,A,C)::(A,A,U)::q -> decode q [Asn]@acc
    | (G,A,C)::(G,A,U)::q -> decode q [Asp]@acc
    | (U,G,C)::(U,G,U)::q -> decode q [Cys]@acc
    | (C,A,A)::(C,A,G)::q -> decode q [Gln]@acc
    | (G,A,A)::(G,A,G)::q -> decode q [Glu]@acc
    | (G,G,A)::(G,G,C)::(G,G,G)::(G,G,U)::q -> decode q [Gly]@acc
    | (C,A,C)::(C,A,U)::q -> decode q [His]@acc
    | (A,U,A)::(A,U,C)::(A,U,U)::q -> decode q [Ile]@acc
    | (C,U,A)::(C,U,C)::(C,U,G)::(C,U,U)::(U,U,A)::(U,U,G)::q -> decode q [Leu]@acc
    | (A,A,A)::(A,A,G)::q -> decode q [Lys]@acc
    | (A,U,G)::q -> decode q [Met]@acc
    | (U,U,C)::(U,U,U)::q -> decode q [Phe]@acc
    | (C,C,C)::(C,C,A)::(C,C,G)::(C,C,U)::q -> decode q [Pro]@acc
    | (U,C,A)::(U,C,C)::(U,C,G)::(U,C,U)::(A,G,U)::(A,G,C)::q -> decode q [Ser]@acc
    | (A,C,A)::(A,C,C)::(A,C,G)::(A,C,U)::q -> decode q [Thr]@acc
    | (U,G,G)::q -> decode q [Trp]@acc
    | (U,A,C)::(U,A,U)::q -> decode q [Tyr]@acc
    | (G,U,A)::(G,U,C)::(G,U,G)::(G,U,U)::q -> decode q [Val]@acc
    | h::q -> decode q acc
  in
  rev (decode triplets [])

let () =
  Random.self_init ();
  let print_nucleobase n = match n with
    | A -> "A" 
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | None -> "None" in
  let rec print_list li = match li with
    | [] -> print_endline ""
    | e::l -> print_string (print_nucleobase e) ; print_list l in
  let rec print_triplets l = match l with
    | [] -> print_endline ""
    | (a,b,c)::q -> print_string "(" ; print_string (print_nucleobase a) ; print_string ", " ; print_string (print_nucleobase b) ; print_string ", " ; print_string (print_nucleobase c) ; print_string ")" ; print_triplets q ;
  in
  let print_aminoacid (a:aminoacid) = match a with
    | Stop -> "Stop"
    | Ala -> "Ala"
    | Arg -> "Arg"
    | Asn -> "Asn"
    | Asp -> "Asp"
    | Cys ->"Cys"
    | Gln -> "Gln"
    | Glu -> "Glu"
    | Gly ->"Gly"
    | His -> "His"
    | Ile -> "Ile"
    | Leu -> "Leu"
    | Lys -> "Lys"
    | Met -> "Met"
    | Phe -> "Phe"
    | Pro -> "Pro"
    | Ser -> "Ser"
    | Thr -> "Thr"
    | Trp -> "Trp"
    | Tyr -> "Tyr"
    | Val -> "Val" in
  let rec print_protein p = match p with
    | [] -> print_endline ""
    | e::l -> print_string (print_aminoacid e) ; print_string " " ; print_protein l in
  let rna5 = generate_rna (generate_helix 5) in
  let triplets5 = generate_bases_triplets rna5 in
  print_string "RNA      : " ; print_list rna5 ;
  print_string "Triplets :" ; print_triplets triplets5 ; print_endline "";
  let rna9 = generate_rna (generate_helix 9) in
  let triplets9 = generate_bases_triplets rna9 in
  print_string "RNA      : " ; print_list rna9 ;
  print_string "Triplets :" ; print_triplets triplets9 ; print_endline "";
  let rna11 = generate_rna (generate_helix 11) in
  let triplets11 = generate_bases_triplets rna11 in
  print_string "RNA      : " ; print_list rna11 ;
  print_string "Triplets :" ; print_triplets triplets11 ; print_endline "";
  let rna_ex = [U; A; C; U; A; U; U; G; G; U; A; C; U; A; U; A; U; G; A; U; G; U; U; C; U; U; U] in
  let triplets_ex = generate_bases_triplets rna_ex in
  let protein_ex = decode_arn rna_ex in
  print_string "RNA      : " ; print_list rna_ex ;
  print_string "Triplets : " ; print_triplets triplets_ex ;
  print_string "AA list  : " ; print_protein protein_ex ;
  print_string "Protein  : " ; print_string (string_of_protein protein_ex) ; print_endline ""; print_endline "";
  let rna_ex = [A; A; C; A; A; U; C; A; C; C; A; U; A; A; A; A; A; G] in
  let triplets_ex = generate_bases_triplets rna_ex in
  let protein_ex = decode_arn rna_ex in
  print_string "RNA      : " ; print_list rna_ex ;
  print_string "Triplets : " ; print_triplets triplets_ex ;
  print_string "AA list  : " ; print_protein protein_ex ;
  print_string "Protein  : " ; print_string (string_of_protein protein_ex) ; print_endline ""; print_endline "";
  print_endline "With STOP : ";
  let rna_ex = [A; A; C; A; A; U; C; A; C; C; A; U; U; A; A; U; A; G; U; G; A; A; A; A; A; A; G] in
  let triplets_ex = generate_bases_triplets rna_ex in
  let protein_ex = decode_arn rna_ex in
  print_string "RNA      : " ; print_list rna_ex ;
  print_string "Triplets : " ; print_triplets triplets_ex ;
  print_string "AA list  : " ; print_protein protein_ex ;
  print_string "Protein  : " ; print_string (string_of_protein protein_ex) ; print_endline ""; 