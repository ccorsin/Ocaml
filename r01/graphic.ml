type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_tama () =
    begin
      let color = Graphics.rgb 71 117 209 in
      Graphics.set_color color ;
      Graphics.fill_rect 450 450 25 25 ;
      Graphics.fill_rect 550 450 25 25 ;
      Graphics.fill_rect 575 475 25 50 ;
      Graphics.fill_rect 525 475 25 25 ;
      Graphics.fill_rect 475 475 25 25 ;
      Graphics.fill_rect 475 500 75 25 ;
      Graphics.fill_rect 425 475 25 50 ;
      Graphics.fill_rect 400 525 25 25 ;
      Graphics.fill_rect 375 550 25 75 ;
      Graphics.fill_rect 325 625 75 25 ;
      let color = Graphics.rgb 31 61 122 in
      Graphics.set_color color ;
      Graphics.fill_rect 300 650 25 25 ;
      Graphics.fill_rect 325 675 75 25 ;
      Graphics.fill_rect 300 700 25 25 ;
      Graphics.fill_rect 325 725 75 25 ;
      Graphics.fill_rect 400 750 25 25 ;
      Graphics.fill_rect 425 725 25 25 ;
      Graphics.fill_rect 425 775 125 25 ;
      Graphics.fill_rect 525 725 25 25 ;
      Graphics.fill_rect 550 750 25 25 ;
      Graphics.fill_rect 575 725 25 25 ;
      Graphics.fill_rect 600 625 25 100 ;
      Graphics.fill_rect 625 550 25 75 ;
      Graphics.fill_rect 600 525 25 25 ;
      Graphics.fill_rect 525 550 25 25 ;
      Graphics.fill_rect 500 575 25 50 ;
      Graphics.fill_rect 550 575 25 50 ;



    end


let main () =
    Graphics.open_graph " 1000x1000";
    Graphics.set_window_title "Tama";
    draw_tama ();
    Graphics.read_key ()

let _ = 
        main ();