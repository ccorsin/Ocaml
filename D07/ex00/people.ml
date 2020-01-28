class people _name =
  object
    val name : string = _name
    val hp : int = 100

    initializer print_endline ("Created object of class people : { name: " ^ name ^ ", hp: " ^ string_of_int hp ^ " }")

    method to_string = "Initialized object of class people : { name: " ^ name ^ ", hp: " ^ string_of_int hp ^ " }"
    method talk = print_endline ("I' m " ^ _name ^ "! Do you know the Doctor?")
    method die = print_endline "Aaaarghh!"

  end