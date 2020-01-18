let rec print_list l = match l with
      | [] -> ()
      | h::t ->
        begin
          print_string h ;
          print_char ' ' ;
          print_list t
        end

let rec fusion_list (l1, l2) = match (l1, l2) with
  | ([],[]) -> []
  | (l1,[]) -> l1
  | ([],l2) -> l2
  | (t::q,_) -> t :: fusion_list (q, l2)

let rec prefix_list l c = match l with
  | [] -> []
  | h::q -> (c ^ h) :: prefix_list q c

let mirror_list l = 
  let rec rev_acc acc t = match t with
  | [] -> acc
  | h::q -> rev_acc (h::acc) q
  in
  rev_acc [] l 

let rec gray n =
  if n < 1 then
    ["Error"]
  else if n = 1 then
    ["0" ; "1"]
  else
    begin
      let l = gray (n - 1) in
      fusion_list ((prefix_list l "0"),(prefix_list (mirror_list l) "1"))
    end

let rec print_list = function 
  [] -> print_endline ""
  | e::l -> print_string e ; print_string " " ; print_list l

let () =
  print_list (gray 1);
  print_list (gray 2);
  print_list (gray 3);
  print_list (gray 4);
  print_list (gray 0);
  print_list (gray (-10));