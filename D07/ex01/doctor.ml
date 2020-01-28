class doctor _name _age _sidekick =
  object
    val _name : string = _name
    val mutable _age : int = _age
    val sidekick : People.people = _sidekick
    val mutable _hp : int = 100

    initializer print_endline ("Created object of class doctor : { name: " ^ _name ^ ", hp: " ^ string_of_int _hp ^ ", age: " ^ string_of_int _age ^ ", sidekick: " ^ sidekick#to_string ^ " }")

    method to_string = "Initialized object of class doctor : { name: " ^ _name ^ ", hp: " ^ string_of_int _hp ^ ", age: " ^ string_of_int _age ^ ", sidekick: " ^ sidekick#to_string ^ " }"
    method talk = print_endline "Hi! I'm the Doctor!"
    method use_sonic_screwdriver = print_endline "Whiiiwhiiiwhiii Whiiiwhiiiwhiii Whiiiwhiiiwhiii"
    method private regenerate = _hp <- 100
    method travel_in_time start arrival = _age <- _age + (arrival - start) ;  print_endline "\x1b[34m
          ___
      ______________
      | \x1b[0mPOLICE      BOX\x1b[34m |
      |_________________|
      | \x1b[0m_____\x1b[34m | \x1b[0m_____\x1b[34m |
      | \x1b[0m|###|\x1b[34m | \x1b[0m|###|\x1b[34m |
      | \x1b[0m|###|\x1b[34m | \x1b[0m|###|\x1b[34m |
      | _____ | _____ |
      | |\x1b[0m###\x1b[34m| | || || |
      | |\x1b[0m###\x1b[34m| | ||_|| |
      | _____ | _____ |
      | || || | || || |
      | ||_|| | ||_|| |
      | _____ | _____ |
      | || || | || || |
      | ||_|| | ||_|| |
      |       |       |
      *****************\x1b[0m"

  end