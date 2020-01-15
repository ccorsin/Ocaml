let leibniz_pi d =
  let pi = 4. *. atan 1. in
  let function_pi i =
    4. *. ((-1.) ** float_of_int(i)) /. ((2. *. float_of_int(i)) +. 1.)
  in
    let rec leibniz_sum s i =
      let diff =
        if (pi -. s) > 0. then
          (pi -. s)
        else
          (s -. pi) in
      if diff < d then
        i
      else
        leibniz_sum (s +. function_pi (i + 1)) (i + 1)
      in
        if d < 0. then
          (-1)
        else
          leibniz_sum (function_pi 0) 0

let () =
  print_int (leibniz_pi (-1.)) ; print_endline "";
  print_int (leibniz_pi (1.)) ; print_endline "";
  print_int (leibniz_pi (0.05)) ; print_endline "";
  print_int (leibniz_pi (1e-5)) ; print_endline ""