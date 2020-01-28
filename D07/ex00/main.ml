let main () =
  let patient = new People.people ("Celia") in
    print_endline ("To_string : " ^ (patient#to_string));
    print_string "Talk      : " ; patient#talk;
    print_string "Die       : " ; patient#die

let () =
  main ()