class virtual alkane n =
  object (self)
    inherit Molecule.molecule
      (( match n with
          | 1 -> "Meth"
          | 2 -> "Eth"
          | 3 -> "Prop"
          | 4 -> "But"
          | 5 -> "Pent"
          | 6 -> "Hex"
          | 7 -> "Hept"
          | 8 -> "Oct"
          | 9 -> "Non"
          | 10 -> "Dec"
          | 11 -> "Undec"
          | 12 -> "Dodec"
          | _ -> "Toobig")
        ^ "ane")
        (( List.init n (fun _ -> (new Atom.carbon :> Atom.atom)) )
        @ (	List.init (2 * n + 2) (fun _ -> (new Atom.hydrogen :> Atom.atom)) ))
        
    method n : int = n
    method to_string :string = "Alkane -> { name : " ^ self#name ^ " ; formula : " ^ self#formula ^ " } "
end

class methane =
  object
    inherit alkane 1
end

class ethane =
  object
    inherit alkane 2
end

class butane =
  object
    inherit alkane 4
end

class octane =
  object
    inherit alkane 8
end