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
    | h::q ->
      begin
        let rec loop ll head = match ll with
          | [] -> get_atoms q (elts@[(1, h)])
          | (i, m)::t ->
            begin
                if (m#equals h) then
                get_atoms q (head@[((i + 1), m)]@t)
                else
                  loop t (head@[(i, m)])
            end
            in loop elts []
          end
    in get_atoms atoms []

let rec print_list l = match l with
  | (m, i)::q -> (print_int i); print_string (" " ^ m#to_string) ; print_list q
  | [] -> print_endline ""

let rec get_all_atoms (atoms : (Atom.atom * int) list) i acc = match atoms with
  | [] -> acc
  | (m, n)::q -> get_all_atoms q i [(m, (n * i))]@acc
  
let count_atoms (molecules : (Molecule.molecule * int) list) acc =
  let rec aggregate_atoms l acc = match l with
    | [] -> acc
    | (m, i)::q -> 
      begin
        (* let atoms = get_all_atoms (m#atoms) i [] in *)
        aggregate_atoms q acc@m#atoms
      end
  in
  (* clean_list_atoms (aggregate_atoms molecules []) *)
  aggregate_atoms molecules []

class alkane_combustion (alkanes : Alkane.alkane list) =
  object (self)
  
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
          let z = ((2 * self#count_C) + self#count_H) / 2 in
          (self#count_molecules) @ [(new Molecule.dioxygen, z)]
    method get_result : (Molecule.molecule * int) list =
          [(new Molecule.carbon_dioxyde, self#count_C) ; (new Molecule.water, (self#count_H / 2))]
    (* method balance = [] *)
    (* method is_balanced : bool = *)
      (* count_atoms self#get_start = count_atoms self#get_result *)

end