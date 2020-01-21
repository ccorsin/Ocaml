let add_jokes a =
  a.(0) <- "What happens when eight Hobbits get together? They turn into a Hobbyte.";
  a.(1) <- "IT paradox? The warmer a computer becomes, the more it freezes.";
  a.(2) <- "A programmer gets shopping instructions from his wife: Go buy a cauliflower. If they have oranges, get two dozens. He comes home with 24 cauliflowers.";
  a.(3) <- "Chuck Norris rewrote the periodic table. He added the element of surprise.";
  a.(4) <- "What's the best thing about Switzerland? I don't know, but their flag is a huge plus.";
  a.(5) <- "How many programmers does it take to change a light bulb? None. It's a hardware problem."

let main () =
  let jokes_base = Array.make 6 "" in
    add_jokes jokes_base;
    let joke = jokes_base.(Random.int 6) in
      print_endline joke

let () =
  Random.self_init ();
  main ()