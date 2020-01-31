class virtual reaction (mol1 : Molecule.molecule list) (mol2 : Molecule.molecule list) =
  object
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool
end