
let tam_from_button st tam tb = 
	   let new_tb = Unix.time () in
	   if ((st.Graphics.button) && (new_tb -. tb) > 0.5) then ( 
        	let new_tam = (Draw.get_action tam st.Graphics.mouse_x st.Graphics.mouse_y) in
        	Graphics.clear_graph (); 
        	Draw.screen new_tam;
        	(new_tam, new_tb)
        )
    	else (tam, tb)


let gestion_clavier st tam = 
    (if (st.Graphics.keypressed && st.Graphics.key = 's')
    then (
  		Graphics.clear_graph ();
    	Draw.save ();
    	ignore (Graphics.read_key ());
    	ignore (Graphics.read_key ());
  		File.save_file (tam);
    	exit 0
    )
    else if (st.Graphics.keypressed && (int_of_char st.Graphics.key) = 27)
    then exit 0 
	else if (st.Graphics.keypressed && Graphics.read_key () = 'p')
	then (
		Graphics.clear_graph ();
		Draw.tamagoshi false;
    	ignore (Graphics.read_key ())
	) else ())


let main () =
  Random.self_init();
  Graphics.open_graph " 1000x1000";
  Graphics.set_window_title "Tama";

  let tama = (new Tama.tama (File.load_file ())) in 
  let colors = [Graphics.rgb 153 51 51 ; Graphics.rgb 153 51 102 ; Graphics.rgb 153 51 153 ; Graphics.rgb 102 51 153 ; Graphics.rgb 51 51 153 ; Graphics.rgb 51 102 153 ; Graphics.rgb 51 153 153 ; Graphics.rgb 51 153 102 ; Graphics.rgb 51 153 51 ; Graphics.rgb 128 153 51 ; Graphics.rgb 153 128 51 ; Graphics.rgb 153 77 51 ] in
  Draw.screen tama;
  try
    let rec loop t tb tam =
      (
	        let st = Graphics.wait_next_event [Graphics.Key_pressed ; Graphics.Poll] in
	        Graphics.synchronize ();
	        Graphics.auto_synchronize true;

	        if (tam#game_over = true) then (
	      		Graphics.clear_graph ();
	      		Draw.game_over ();
      			(if (Graphics.read_key () = 'y')
      			then loop t tb (new Tama.tama (100, 100, 100, 100))
      			else exit 0)
      		);

	        gestion_clavier st tam ;
	        let (tam1, tb1) = tam_from_button st tam tb in
	        let time = Unix.time () in
	        if (time -. t) > 1. then (
	          
	          let new_tam = tam1#get_old in
	          let color = List.nth colors (Random.int 11) in
	          Graphics.set_color color ;
	          Graphics.clear_graph ();
	          Draw.screen new_tam;
	          loop time tb1 new_tam
	        )
	        else loop t tb1 tam1
      )
    in loop (Unix.time ()) (Unix.time ()) tama;
    ignore (Graphics.read_key ())
  with Exit -> ()

let () = 
      main ();







