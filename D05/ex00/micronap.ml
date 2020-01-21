let my_sleep ()= Unix.sleep 1

let main () =
  if Array.length Sys.argv = 2 then
    let time = try int_of_string (Sys.argv.(1)) with 
      | _ -> 0
    in
      begin
        for i = 0 to (time - 1) do
          my_sleep ()
        done
      end


let () =
  main ()


(* ocamlopt unix.cmxa micronap.ml*)