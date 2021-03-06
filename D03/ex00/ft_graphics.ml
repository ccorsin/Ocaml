type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
  if size <> 0 then
    begin
      Graphics.moveto (x - size / 2) (y - size / 2) ;
      Graphics.lineto (x + size / 2) (y - size / 2) ;
      Graphics.lineto (x + size / 2) (y + size / 2) ;
      Graphics.lineto (x - size / 2) (y + size / 2) ;
      Graphics.lineto (x - size / 2) (y - size / 2) ;
    end


let draw_tree_node node = 
  let rec draw_tree n x y = match n with
    | Node (v, l, r) -> begin
                          draw_square x y 50 ;
                          Graphics.moveto (x - 50 / 5) (y - 50 / 5) ;
                          Graphics.draw_string v ; 
                          Graphics.moveto (x + 50 / 2) (y) ;
                          Graphics.lineto (x + 100) (y - 50) ; 
                          Graphics.moveto (x + 50 / 2) (y) ;
                          Graphics.lineto (x + 100) (y + 50) ; 
                          draw_tree r (x + 125) (y - 50) ;
                          draw_tree l (x + 125) (y + 50) ;
                        end 
    | Nil -> begin
              draw_square x y 50 ;
              Graphics.moveto (x - 50 / 5) (y - 50 / 5) ;
              Graphics.draw_string "Nil" ;
            end
  in draw_tree node 200 300



let main () =
    Graphics.open_graph " 600x600";
    Graphics.set_window_title "The square & the tree";
    draw_square 100 100 100;
    draw_tree_node (Node ("42", Node("24", Nil, Nil), Nil));
    Graphics.read_key ()

let _ = 
        main ();