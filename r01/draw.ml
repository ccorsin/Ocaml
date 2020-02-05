let game_over () = (
	let color = Graphics.rgb 71 117 209 in
		Graphics.set_color color ;
		Graphics.fill_rect 150 700 75 25 ;
		Graphics.fill_rect 125 725 25 25 ;
		Graphics.fill_rect 100 750 25 100 ;
		Graphics.fill_rect 125 850 25 25 ;
		Graphics.fill_rect 150 875 75 25 ;
		Graphics.fill_rect 225 850 25 25 ;
		Graphics.fill_rect 225 725 25 50 ;
		Graphics.fill_rect 175 775 50 25 ;

	let color = Graphics.rgb 0 0 77 in
		Graphics.set_color color ;
		Graphics.fill_rect 275 700 25 150 ;
		Graphics.fill_rect 300 850 25 25 ;
		Graphics.fill_rect 325 875 50 25 ;
		Graphics.fill_rect 375 850 25 25 ;
		Graphics.fill_rect 400 700 25 150 ;
		Graphics.fill_rect 300 775 100 25 ;

	let color = Graphics.rgb 71 117 209 in
		Graphics.set_color color ;
		Graphics.fill_rect 450 700 25 200 ;
		Graphics.fill_rect 475 850 25 25 ;
		Graphics.fill_rect 500 825 50 25 ;
		Graphics.fill_rect 550 850 25 25 ;
		Graphics.fill_rect 575 700 25 200 ;

	let color = Graphics.rgb 0 0 77 in
		Graphics.set_color color ;
		Graphics.fill_rect 625 700 75 25 ;
		Graphics.fill_rect 700 725 25 25 ;
		Graphics.fill_rect 625 725 25 150 ;
		Graphics.fill_rect 625 875 75 25 ;
		Graphics.fill_rect 700 850 25 25 ;
		Graphics.fill_rect 650 785 50 25 ;


	let color = Graphics.rgb 71 117 209 in
		Graphics.set_color color ;
		Graphics.fill_rect 300 300 25 150 ;
		Graphics.fill_rect 325 275 75 25 ;
		Graphics.fill_rect 325 450 75 25 ;
		Graphics.fill_rect 400 300 25 150 ;

	let color = Graphics.rgb 0 0 77 in
		Graphics.set_color color ;
		Graphics.fill_rect 500 275 50 25 ;
		Graphics.fill_rect 475 300 25 100 ;
		Graphics.fill_rect 450 400 25 75 ;
		Graphics.fill_rect 550 300 25 100 ;
		Graphics.fill_rect 575 400 25 75 ;

	let color = Graphics.rgb 71 117 209 in
		Graphics.set_color color ;
		Graphics.fill_rect 625 275 75 25 ;
		Graphics.fill_rect 700 300 25 25 ;
		Graphics.fill_rect 625 300 25 150 ;
		Graphics.fill_rect 625 450 75 25 ;
		Graphics.fill_rect 700 425 25 25 ;
		Graphics.fill_rect 650 365 50 25 ;

	let color = Graphics.rgb 0 0 77 in
		Graphics.set_color color ;
		Graphics.fill_rect 750 275 25 175 ;
		Graphics.fill_rect 775 450 50 25 ;
		Graphics.fill_rect 825 400 25 50 ;
		Graphics.fill_rect 800 375 25 25 ;
		Graphics.fill_rect 775 350 25 25 ;
		Graphics.fill_rect 800 325 25 25 ;
		Graphics.fill_rect 825 275 25 50 ;

	Graphics.moveto 200 150;
	Graphics.draw_string "Restart ? y/n ";

)

let save () = (
	let color = Graphics.rgb 71 117 209 in
		Graphics.set_color color ;
		Graphics.fill_rect 200 450 75 25 ;
		Graphics.fill_rect 275 475 25 75 ;
		Graphics.fill_rect 225 550 50 25 ;
		Graphics.fill_rect 200 575 25 50 ;
		Graphics.fill_rect 225 625 75 25 ;

	let color = Graphics.rgb 0 0 77 in
		Graphics.set_color color ;
		Graphics.fill_rect 350 450 25 150 ;
		Graphics.fill_rect 375 600 25 25 ;
		Graphics.fill_rect 400 625 25 25 ;
		Graphics.fill_rect 425 600 25 25 ;
		Graphics.fill_rect 450 450 25 150 ;
		Graphics.fill_rect 350 525 100 25 ;

	let color = Graphics.rgb 71 117 209 in
		Graphics.set_color color ;
		Graphics.fill_rect 575 450 50 25 ;
		Graphics.fill_rect 550 475 25 100 ;
		Graphics.fill_rect 525 575 25 75 ;
		Graphics.fill_rect 625 475 25 100 ;
		Graphics.fill_rect 650 575 25 75 ;

	let color = Graphics.rgb 0 0 77 in
		Graphics.set_color color ;
		Graphics.fill_rect 725 450 75 25 ;
		Graphics.fill_rect 800 475 25 25 ;
		Graphics.fill_rect 725 475 25 150 ;
		Graphics.fill_rect 725 625 75 25 ;
		Graphics.fill_rect 800 600 25 25 ;
		Graphics.fill_rect 750 540 50 25 ;
)

let meter title level x y = (

	Graphics.moveto x (y + 50);
	Graphics.draw_string title;

	Graphics.fill_rect x y level 25;

	Graphics.moveto x (y + 12);
	Graphics.lineto (x + 100) (y + 12)
)

let button title x y b = (

	if (b) then (
		let color = Graphics.rgb 179 0 0 in
    	Graphics.set_color color
    );

	Graphics.moveto x y;
	Graphics.lineto (x + 80) y;
	Graphics.lineto (x + 80) (y + 75);
	Graphics.lineto x (y + 75);
	Graphics.lineto x y;

	Graphics.moveto (x + 20) (y + 30);
	Graphics.draw_string title;

	if (b) then (
		let color = Graphics.rgb 0 0 77 in
		Graphics.set_color color
	)
)

let tamagoshi b = 
	let color = Graphics.rgb 71 117 209 in
	Graphics.set_color color ;
	Graphics.fill_rect 450 350 25 25 ;
	Graphics.fill_rect 550 350 25 25 ;
	Graphics.fill_rect 575 375 25 50 ;
	Graphics.fill_rect 525 375 25 25 ;
	Graphics.fill_rect 475 375 25 25 ;
	Graphics.fill_rect 475 400 75 25 ;
	Graphics.fill_rect 425 375 25 50 ;
	Graphics.fill_rect 400 425 25 25 ;
	Graphics.fill_rect 375 450 25 75 ;
	Graphics.fill_rect 325 525 75 25 ;

	let color = Graphics.rgb 0 0 77 in
	Graphics.set_color color ;
	Graphics.fill_rect 425 625 25 25 ;
	Graphics.fill_rect 525 625 25 25; 

	let color = match b with 
		| false -> Graphics.rgb 31 61 122
		| true -> Graphics.rgb 179 0 0
	in
	Graphics.set_color color ;
	Graphics.fill_rect 300 550 25 25 ;
	Graphics.fill_rect 325 575 75 25 ;
	Graphics.fill_rect 300 600 25 25 ;
	Graphics.fill_rect 575 625 25 25 ;
	Graphics.fill_rect 325 625 75 25 ;
	Graphics.fill_rect 400 650 25 25 ;
	Graphics.fill_rect 425 675 125 25 ;
	Graphics.fill_rect 550 650 25 25 ;
	Graphics.fill_rect 600 525 25 100 ;
	Graphics.fill_rect 625 450 25 75 ;
	Graphics.fill_rect 600 425 25 25 ;
	Graphics.fill_rect 525 450 25 25 ;
	Graphics.fill_rect 500 475 25 50 ;
	Graphics.fill_rect 550 475 25 50;
	let color = Graphics.rgb 31 61 122 in
	Graphics.set_color color 

let screen (tama:Tama.tama) = (
	meter "HEALTH" tama#health 150 850;
	meter "HYGIENE" tama#hygiene 350 850;
	meter "ENERGY" tama#energy 550 850;
	meter "HAPPYNESS" tama#happyness 750 850;

	tamagoshi tama#warning;

	button "EAT" 130 150 (tama#action = 1);
	button "THUNDER" 230 150 (tama#action = 2);
	button "BATH" 330 150 (tama#action = 3);
	button "KILL" 430 150 (tama#action = 4);
	button "SLEEP" 530 150 (tama#action = 5);
	button "DANCE" 630 150 (tama#action = 6);
	button "TICKLE" 730 150 (tama#action = 7);

)

let is_on_button px py x y = (x > px && x < (px + 80) && y > py && y < (py + 75))

let get_action (tama: Tama.tama) x y = (

	if ((is_on_button 130 150 x y) = true) then tama#eat 
	else if ((is_on_button 230 150 x y) = true) then tama#thunder
	else if ((is_on_button 330 150 x y;) = true) then tama#bath
	else if ((is_on_button 430 150 x y;) = true) then tama#kill
	else if ((is_on_button 530 150 x y;) = true) then tama#sleep
	else if ((is_on_button 630 150 x y;) = true) then tama#dance
	else if ((is_on_button 730 150 x y;) = true) then tama#tickle
	else tama#clone
)


