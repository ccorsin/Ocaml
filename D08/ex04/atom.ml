class virtual atom name symbol number =
  object (self)
    method name : string = name
    method symbol : string = symbol
    method atomic_number : int = number

    method to_string = "Atom : { name : " ^ self#name ^ " ; symbol : " ^ self#symbol ^ " ; atomic_number : " ^ string_of_int self#atomic_number ^ " }"

    method equals (bench : atom) = (self#atomic_number = bench#atomic_number)

    method value symbol = match symbol with
      | "C" -> 0
      | "H" -> 1
      | _ -> 2
  end

class hydrogen =
  object
    inherit atom "Hydrogen" "H" 1
  end

class carbon =
object
  inherit atom "Carbon" "C" 12
end

class oxygen =
object
  inherit atom "Oxygen" "O" 16
end

class helium =
  object
    inherit atom "Helium" "He" 2
  end

class calcium =
object
  inherit atom "Calcium" "Ca" 20
end

class nitrogen =
object
  inherit atom "Nitrogen" "N" 7
end

class aluminium =
object
  inherit atom "Aluminium" "Al" 13
end