let eu_dist (a:float array) (b:float array) =
  let rec sum_square i acc =
    if i = (Array.length a) then
      acc
    else
      sum_square (i + 1) (acc +. ((a.(i) -. b.(i)) ** 2.))
  in
  sqrt (sum_square 0 0.)
let () =
  let a = [|1.; 1.; 1.; 1.|] in
  let b = [|2.; 2.; 2.; 2.|] in
  print_float (eu_dist a b);
  print_endline "";
  print_float (eu_dist [|4.5; 1.0; (-2.5); 0.4|] [|2.; 3.; 12.; (-42.)|]);
  print_endline "";
  print_float (eu_dist [|1.; (-2.)|] [| 4.; 2.|]);
  print_endline "";