let rec converges (f : 'a -> 'a) x n =
  if n < 0 then
    false
  else if n = 0 && f x <> x then
    false
  else if x = f x then
    true
  else
    converges f (f x) (n - 1)
  
let () =
    let test a = if a then print_endline "true" else print_endline "false" in
    test (converges (( * ) 2) 2 5);
    test (converges (fun x -> x / 2) 2 3);
    test (converges (fun x -> x / 2) 2 2);
    test (converges (fun x -> x / 2) 2 (-42));
    test (converges (fun x -> x / 2) (-2) 2);