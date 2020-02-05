let gray n =
  if n < 1 then
    print_endline "Error"
  else
    let rec build_gray n =
      if n = 1 then
        ["0" ; "1"]
      else
        begin
          let l = build_gray (n - 1) in

          let rec fusion_list (l1, l2) = match (l1, l2) with
            | ([],[]) -> []
            | (l1,[]) -> l1
            | ([],l2) -> l2
            | (t::q,_) -> t :: fusion_list (q, l2) in

          let rec prefix_list l3 c = match l3 with
            | [] -> []
            | h::q -> (c ^ h) :: prefix_list q c in

          let mirror_list l4 = 
              let rec rev_acc acc t = match t with
                | [] -> acc
                | h::q -> rev_acc (h::acc) q
              in
              rev_acc [] l4
          in
          fusion_list ((prefix_list l "0"),(prefix_list (mirror_list l) "1"))
        end
      in
      let rec print_list li = match li with
        | [] -> print_endline ""
        | e::l -> print_string e ; print_string " " ; print_list l in
      print_list (build_gray n)

let () =
  gray 1;
  gray 2;
  gray 3;
  gray 4;
  gray 0;
  gray (-10);