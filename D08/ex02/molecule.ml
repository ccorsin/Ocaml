let compare_atoms (a : int * Atom.atom) (b : int * Atom.atom) =
  let (i_a, a_atom) = a in
  let (i_b, b_atom) = b in
  if a_atom#symbol = "C" || b_atom#symbol = "C" || a_atom#symbol = "H" || b_atom#symbol = "H" then
    (a_atom#value a_atom#symbol) - (b_atom#value b_atom#symbol)
  else
    String.compare a_atom#symbol b_atom#symbol

let rec print_list l = match l with
  | (i, a)::q -> (print_int i); print_list q
  | [] -> print_endline ""

let generate_formula atoms : string  =
  let rec count_atoms l elts = match l with
    | [] -> elts
    | h::q -> 
      begin
        let rec loop ll head = match ll with
          | [] -> count_atoms q (elts@[(1, h)])
          | (i, a)::t ->
            begin
                if (a#equals h) then
                  count_atoms q (head@[((i + 1), a)]@t)
                else
                  loop t (head@[(i, a)])
            end
            in loop elts []
      end
  in
  let ll = List.sort compare_atoms (count_atoms atoms []) in
  let rec generate_string l acc = match l with
      | [] -> acc
      | (i, a)::q ->
          if i <> 1 then generate_string q (acc ^ a#symbol ^ string_of_int i)
          else generate_string q (acc ^ a#symbol)
  in
  generate_string ll ""

class virtual molecule name (atoms : Atom.atom list) =
  object (self)
    method name : string = name
    method formula : string = generate_formula atoms

    method to_string = "Molecule -> { name : " ^ self#name ^ " ; formula : " ^ self#formula ^ " } "
    method equals (bench : molecule) = (self#name = bench#name) && (self#formula = bench#formula)
  end

class water =
  object
    inherit molecule "Water" [new Atom.hydrogen ; new Atom.hydrogen ; new Atom.oxygen]
end

class carbon_dioxyde =
  object
    inherit molecule "Carbon dioxyde" [new Atom.oxygen ; new Atom.oxygen ; new Atom.carbon ]
end

class  nitrogen_monoxide =
object
  inherit molecule "Nitric oxide" [new Atom.oxygen ; new Atom.nitrogen]
end

class  calcium_carbonate =
object
  inherit molecule "Calcium carbonate" [new Atom.calcium ; new Atom.oxygen ; new Atom.carbon ; new Atom.oxygen ; new Atom.oxygen]
end

class  aluminium_oxide =
object
  inherit molecule "Aluminium oxide" [new Atom.oxygen ; new Atom.oxygen ; new Atom.aluminium ; new Atom.oxygen ; new Atom.aluminium]
end