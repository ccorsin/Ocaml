module SHash =
  struct
    type t = string

    let equal s1 s2 = (s1 = s2)

    let hash s = 
      let rec hash_aux i acc h x  =
          if i = (String.length s) then
            acc
          else
            hash_aux (i + 1) ((int_of_char (String.get s i) lxor h) + acc) (h * x) x
      in hash_aux 0 424242 2166136261 16777619
  end

module StringHashtbl = Hashtbl.Make (SHash)
let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht