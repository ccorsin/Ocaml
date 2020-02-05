type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec height tree = match tree with
  | Nil -> -1
  | Node (_, l, r) -> 1 + (max (height l) (height r))

let is_bst tree =
  let rec check_tree t v_l v_r = match t with
    | Nil -> true
    | Node (v, l, r) -> (v > v_l) && (v < v_r) && (check_tree l v_l v) && (check_tree r v v_r)
  in
  check_tree tree min_int max_int

let is_perfect tree =
  let h = height tree in
  let rec check_tree t d i = match t with
    | Nil -> true
    | Node (_, l, r) ->
      if (l = Nil) && (r = Nil) then (d = i)
      else if (l = Nil) || (r = Nil) then false
      else (check_tree l d (i + 1)) && (check_tree r d (i + 1))
  in
  check_tree tree h 0

let rec is_balanced tree = match tree with
  | Nil -> true
  | Node (v, l, r) -> 
    if ((abs ((height l) - (height r))) <= 1) && (is_balanced l) && (is_balanced r) then true
    else false

let rec search_bst x tree = match tree with
  | Nil -> false
  | Node (v, l, r) ->
    if (v = x) then true
    else (search_bst x l) || (search_bst x r)

let rec add_bst x tree = match tree with
  | Nil -> Node (x, Nil, Nil)
  | Node (v, l, r) -> 
    if (x < v) then Node (v, (add_bst x l), r)
    else if (x > v) then Node (v, l, (add_bst x r))
    else Node (v, l, r)

let rec find_max tr = match tr with
  | Nil -> assert false
  | Node (v, _, Nil) -> v
  | Node (v, _, r) -> find_max r

let rec delete_bst x t = match t with
  | Nil -> Nil
  | Node (v, l, r) -> 
    if v = x then join_trees l r
    else if x < v then Node (v, (delete_bst x l), r)
    else Node (v, l, (delete_bst x r))
and join_trees l r = match l, r with
  | Nil, r -> r
  | l, Nil -> l
  | l, r -> let m = find_max l in Node (m, (delete_bst m l), r)


let rec print_tree tree = match tree with
  | Nil -> print_string "Nil"
  | Node (v, l, r) -> print_string (string_of_int v) ; print_string " [ " ; print_tree l ; print_string " ] " ; print_string " ( " ; print_tree r ; print_string " ) "

let () =
  print_endline "IS_BST ?";
  print_endline "Initial tree : ";
  let tree = Node (42, Node(40, Nil, Nil), Node(45, Node (43, Nil, Nil), Node(20, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  if is_bst tree then print_endline "true"
  else print_endline "false" ;
  print_endline "Initial tree : ";
  let tree = Node (42, Node(40, Nil, Nil), Node(45, Node (43, Nil, Nil), Node(50, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  if is_bst tree then print_endline "true"
  else print_endline "false"; print_endline "" ;
  print_endline "IS_PERFECT ?";
  print_endline "Initial tree : ";
  let tree = Node (42, Node(38, Node(35, Nil, Nil), Node(40, Nil, Nil)), Node(45, Node(43, Nil, Nil), Node(47, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  if is_perfect tree then print_endline "true"
  else print_endline "false" ;
  print_endline "Initial tree : ";
  let tree = Node (42, Node(38, Nil, Nil), Node(45, Node (43, Nil, Nil), Node(50, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  if is_perfect tree then print_endline "true"
  else print_endline "false"; print_endline "" ;
  print_endline "IS_BALANCED ?";
  print_endline "Initial tree : ";
  let tree = Node (1, Node(2, Node(4, Node(8, Nil, Nil), Nil), Node(5, Nil, Nil)), Node(3, Nil, Nil)) in
  print_tree tree ; print_endline "" ;
  if is_balanced tree then print_endline "true"
  else print_endline "false" ;
  print_endline "Initial tree : ";
  let tree = Node (1, Node(2, Node(4, Node(7, Nil, Nil), Nil), Node(5, Nil, Nil)), Node(3, Node (6, Nil, Nil), Nil)) in
  print_tree tree ; print_endline "" ;
  if is_balanced tree then print_endline "true"
  else print_endline "false" ; print_endline "" ;
  print_endline "IS_THERE_VALUE 7 ?";
  print_endline "Initial tree : ";
  let tree = Node (1, Node(2, Node(4, Node(8, Nil, Nil), Nil), Node(5, Nil, Nil)), Node(3, Nil, Nil)) in
  print_tree tree ; print_endline "" ;
  if search_bst 7 tree then print_endline "true"
  else print_endline "false" ;
  print_endline "Initial tree : ";
  let tree = Node (1, Node(2, Node(4, Node(7, Nil, Nil), Nil), Node(5, Nil, Nil)), Node(3, Node (6, Nil, Nil), Nil)) in
  print_tree tree ; print_endline "" ;
  if search_bst 7 tree then print_endline "true"
  else print_endline "false" ; print_endline "" ;
  print_endline "ADD VALUE ?";
  print_endline "Initial tree : ";
  let tree = Node (42, Node(38, Node(35, Nil, Nil), Node(40, Nil, Nil)), Node(45, Node(43, Nil, Nil), Node(47, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  print_string "Add 25 : " ; print_endline "" ;
  print_tree (add_bst 25 tree) ; print_endline ""; print_endline ""; print_endline "";
  print_endline "Initial tree : ";
  let tree = Node (42, Node(38, Node(35, Nil, Nil), Node(40, Nil, Nil)), Node(45, Node(43, Nil, Nil), Node(47, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  print_string "Add 46 : " ; print_endline "" ;
  print_tree (add_bst 46 tree) ; print_endline ""; print_endline "";
  print_endline "REMOVE VALUE ?";
  print_endline "Initial tree : ";
  let tree = Node (42, Node(38, Node(35, Nil, Nil), Node(40, Nil, Nil)), Node(45, Node(43, Nil, Nil), Node(47, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  print_string "Remove 35 : " ; print_endline "" ;
  print_tree (delete_bst 35 tree) ; print_endline ""; print_endline "";
  print_endline "Initial tree : ";
  let tree = Node (42, Node(38, Node(35, Nil, Nil), Node(40, Nil, Nil)), Node(45, Node(43, Nil, Nil), Node(47, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  print_string "Remove 47 : " ; print_endline "" ;
  print_tree (delete_bst 47 tree) ; print_endline ""; print_endline "";
  print_endline "Initial tree : ";
  let tree = Node (42, Node(38, Node(35, Nil, Nil), Node(40, Nil, Nil)), Node(45, Node(43, Nil, Nil), Node(47, Nil, Nil))) in
  print_tree tree ; print_endline "" ;
  print_string "Remove 45 : " ; print_endline "" ;
  print_tree (delete_bst 45 tree) ; print_endline ""