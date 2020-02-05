type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size tree = match tree with
  | Nil -> 0
  | Node (_, l, r) -> 1 + (size l) + (size r)

let rec height tree = match tree with
  | Nil -> -1
  | Node (_, l, r) -> 1 + (max (height l) (height r))

let draw_square x y size =
  if size <> 0 then
    begin
      Graphics.moveto (x - size / 2) (y - size / 2) ;
      Graphics.lineto (x + size / 2) (y - size / 2) ;
      Graphics.lineto (x + size / 2) (y + size / 2) ;
      Graphics.lineto (x - size / 2) (y + size / 2) ;
      Graphics.lineto (x - size / 2) (y - size / 2) ;
    end

let draw_tree tree =
  let h = (height tree + 2) in
  let rec ft_draw_tree tree h x y i = match tree with
  | Node (v, l, r) -> begin
    draw_square x y (700 / (2 * h)) ;
    Graphics.moveto (x - (700 / (2 * h)) / 5) (y - (700 / (2 * h)) / 5) ;
    Graphics.draw_string v ;
    Graphics.moveto (x + (700 / (4 * h))) (y) ;
    Graphics.lineto (x + (700 / h)) (y - 150 + (i * 50)) ; 
    Graphics.moveto (x + (700 / (4 * h))) (y) ;
    Graphics.lineto (x + (700 / h)) (y + 150 - (i * 50)) ; 
    ft_draw_tree r h (x + (700 / h) + (700 / (4 * h))) (y - 150 + (i * 50)) (i + 1);
    ft_draw_tree l h (x + (700 / h) + (700 / (4 * h))) (y + 150 - (i * 50)) (i + 1);
  end 
  | Nil -> begin
    draw_square x y (700 / (2 * h)) ;
    Graphics.moveto (x - (700 / (2 * h)) / 5) (y - (700 / (2 * h)) / 5) ;
    Graphics.draw_string "Nil" ;
    end
  in
  ft_draw_tree tree h 50 400 0

let main () =
  begin
    let tree = Node ("Root", Nil, Nil) in
    print_string "TREE 1 size   : " ; print_int (size tree) ;
    print_endline ("") ;
    print_string "TREE 1 height : " ; print_int (height tree) ;
    print_endline ("") ;
    let tree = Node ("Node", Node("Leaf", Nil, Nil), Node("Node", Nil, Node("Node", Node("Leaf", Nil, Nil), Node("Leaf", Nil, Nil)))) in
    print_string "TREE 2 size   : " ; print_int (size tree) ;
    print_endline ("") ;
    print_string "TREE 2 height : " ; print_int (height tree) ;
    print_endline ("") ;
    print_endline ("TREE 3") ; 
    let tree = Node ("Node", Node("Leaf", Nil, Nil), Node("Node", Nil, Node("Leaf", Nil, Nil))) in
    print_string "size   : " ; print_int (size tree) ;
    print_endline ("") ;
    print_string "height : " ; print_int (height tree) ;
    print_endline ("") ; 
    Graphics.open_graph " 800x800";
    Graphics.set_window_title "The tree";
    draw_tree tree;
    Graphics.read_key ()
    end

let _ = 
        main ();