
let safe f in_channel =
	try Some (f in_channel)
	with | _ -> None

let nb_lines in_channel = 
	let rec loop i = match (safe input_line in_channel) with
			| None -> i
			| _ -> loop (i + 1)
	in loop 0

let load_file () = 
	try
		let ic = open_in ("save.itama") in
		let nb = (nb_lines ic) in
		if (nb <> 4) then (100, 100, 100, 100)
		else (
			seek_in ic 0;
			let param = ((int_of_string (input_line ic)), 
			(int_of_string (input_line ic)), (int_of_string (input_line ic)), 
			(int_of_string (input_line ic))) in
			close_in ic;
			param
		)
	with
		| Sys_error err -> print_endline "No previous game loaded.";(100, 100, 100, 100)
		| _ -> (100, 100, 100, 100)

let save_file (tam: Tama.tama) = 
	try
		let oc = open_out ("save.itama") in
		output_string oc ((string_of_int tam#health) ^ "\n");
		output_string oc ((string_of_int tam#hygiene) ^ "\n");
		output_string oc ((string_of_int tam#energy) ^ "\n");
		output_string oc ((string_of_int tam#happyness) ^ "\n");
		close_out oc
	with
		| Sys_error err -> print_endline err
		| _ -> ()

