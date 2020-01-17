type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size tree = match tree with
  | Nil -> 1
  | Node (_, l, r) -> 1 + (size l) + (size r)

let height tree = match tree with
  | Nil -> 1
  | Node (_, l, r) -> 1 + (max (size l) (size r))

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
  let h = height tree in
  let rec ft_draw_tree tree h x y = match tree with
  | Node (v, l, r) -> begin
    draw_square x y (700 / (2 * h)) ;
    Graphics.moveto (x - (700 / (2 * h)) / 5) (y - (700 / (2 * h)) / 5) ;
    Graphics.draw_string v ; 
    Graphics.moveto (x + (700 / (4 * h))) (y) ;
    Graphics.lineto (x + (700 / h)) (y - 50) ; 
    Graphics.moveto (x + (700 / (4 * h))) (y) ;
    Graphics.lineto (x + (700 / h)) (y + 50) ; 
    ft_draw_tree r h (x + (700 / h) + (700 / (4 * h))) (y - 50) ;
    ft_draw_tree l h (x + (700 / h) + (700 / (4 * h))) (y + 50) ;
  end 
  | Nil -> begin
    draw_square x y (700 / (2 * h)) ;
    Graphics.moveto (x - (700 / (2 * h)) / 5) (y - (700 / (2 * h)) / 5) ;
    Graphics.draw_string "Leaf" ;
    end
  in
  ft_draw_tree tree h 50 400

let main () =
  begin
  (* print_int (size (Node ("42", Node("24", Nil, Nil), Node("12", Nil, Nil)))) ;
  print_endline ("") ;
  print_int (height (Node ("42", Node("24", Nil, Nil), Node("12", Nil, Nil)))) ;
  print_endline ("") ;  *)
  Graphics.open_graph " 800x800";
  Graphics.set_window_title "The tree";
  draw_tree (Node ("Node", Node("Nod", Nil, Nil), Node("Nod", Nil, Node("Nod", Nil, Nil))));
  Graphics.read_key ()
  end

let _ = 
        main ();