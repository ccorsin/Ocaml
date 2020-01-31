exception Unbalanced of string

let clean_list molecules =
  let rec count_molecules l elts = match l with
    | [] -> elts
    | h::q ->
      begin
        let rec loop ll head = match ll with
          | [] -> count_molecules q (elts@[(h, 1)])
          | (m, i)::t ->
            begin
                if ((m :> Molecule.molecule)#equals (h :> Molecule.molecule)) then
                count_molecules q (head@[(m, (i + 1))]@t)
                else
                  loop t (head@[(m, i)])
            end
            in loop elts []
          end
    in count_molecules molecules []

let clean_list_atoms atoms =
  let rec get_atoms l elts = match l with
    | [] -> elts
    | (n, h)::q ->
      begin
        let rec loop ll head = match ll with
          | [] -> get_atoms q (elts@[(n, h)])
          | (i, a)::t ->
            begin
                if (a#equals h) then
                  get_atoms q (head@[((i + n), a)]@t)
                else
                  loop t (head@[(i, a)])
            end
            in loop elts []
          end
    in get_atoms atoms []

let rec print_list l = match l with
  | (m, i)::q -> (print_int i); print_string (" " ^ m#to_string) ; print_list q
  | [] -> print_endline ""

let rec get_all_atoms (atoms : (int * Atom.atom) list) i acc = match atoms with
  | [] -> acc
  | (n, a)::q -> get_all_atoms q i [((n * i), a)]@acc
  
let count_atoms (molecules : (Molecule.molecule * int) list) acc =
  let rec aggregate_atoms l acc = match l with
    | [] -> acc
    | (m, i)::q -> 
      begin
        let atoms = get_all_atoms (m#atoms) i [] in
        aggregate_atoms q acc@atoms
      end
  in
  clean_list_atoms (aggregate_atoms molecules [])

let double_molecules (molecules : (Molecule.molecule * int) list) =
  let rec double_loop l acc = match l with
    | [] -> acc
    | (m, i)::q -> double_loop q [(m, (i * 2))]@acc
  in double_loop molecules []

let rec adjust_alkanes_list (alkanes : Alkane.alkane list) i acc = match i with
  | 0 -> acc
  | _ -> adjust_alkanes_list alkanes (i - 1) (alkanes@acc)

class alkane_combustion (alkanes : Alkane.alkane list) =
  object (self)
    inherit Reaction.reaction ((alkanes :> Molecule.molecule list)@[new Molecule.dioxygen]) [new Molecule.water; new Molecule.carbon_dioxyde]
  
    method count_alkanes : (Alkane.alkane * int) list = clean_list alkanes
    method count_molecules : (Molecule.molecule * int) list = clean_list (alkanes :> Molecule.molecule list)
    method count_C : int = 
      let l = self#count_alkanes in
      let rec count ll acc = match ll with
        | [] -> acc
        | (m, i)::q -> count q (acc + (i * m#n))
      in count l 0
    method count_H : int = 
      let l = self#count_alkanes in
      let rec count ll acc = match ll with
        | [] -> acc
        | (m, i)::q -> count q (acc + (i * ((m#n)* 2 + 2)))
      in count l 0
    method get_start : (Molecule.molecule * int) list = 
        let z = (((2 * self#count_C) + (self#count_H / 2))) in
        if (z mod 2 = 0) then (self#count_molecules) @ [(new Molecule.dioxygen, (z / 2))]
        else (double_molecules (self#count_molecules)) @ [(new Molecule.dioxygen, z)]
    method get_result : (Molecule.molecule * int) list =
        let z = (((2 * self#count_C) + (self#count_H / 2))) in
        if (z mod 2 = 0) then  [(new Molecule.carbon_dioxyde, self#count_C) ; (new Molecule.water, (self#count_H / 2))]
        else [(new Molecule.carbon_dioxyde, ((self#count_C) * 2)) ; (new Molecule.water, (self#count_H))]
         
    method atoms_result : (int * Atom.atom) list = count_atoms (self#get_result) []
    method atoms_start : (int * Atom.atom) list = count_atoms (self#get_start) []
    method balance =
      let z = (((2 * self#count_C) + (self#count_H / 2))) in
        if (z mod 2 = 0) then ((new alkane_combustion alkanes) :> Reaction.reaction)
        else ((new alkane_combustion (adjust_alkanes_list alkanes 2 [])) :> Reaction.reaction)
    method is_balanced = 
      let rec compare_atoms l1 l2 = match (l1, l2) with
        | ([],[]) -> true
        | ([],_) -> false
        | (_,[]) -> false
        | ((i1, a1)::q1, (i2, a2)::q2) ->
          if (i1 = i2) && (a1#equals a2) then compare_atoms q1 q1
          else false
      in compare_atoms (count_atoms (self#get_start) []) (count_atoms (self#get_result) [])

end