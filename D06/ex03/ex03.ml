module type FRACTIONNAL_BITS =
  sig
    val bits : int
  end

module type FIXED = sig 
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min  : t -> t-> t
  val max  : t -> t -> t
  val gth  : t -> t -> bool
  val lth  : t -> t -> bool
  val gte  : t -> t -> bool
  val lte  : t -> t -> bool
  val eqp  : t -> t -> bool
  val eqs  : t -> t -> bool
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val div  : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type MAKE =
  functor ( FractionnalBits : FRACTIONNAL_BITS ) -> FIXED

module Make : MAKE =
  functor ( FractionnalBits : FRACTIONNAL_BITS ) ->
    struct
      type t = int

      let of_int (i:int) = i lsl FractionnalBits.bits
      let to_int t = t lsr FractionnalBits.bits
      let of_float x = int_of_float (floor (0.5 +. x *. (float_of_int (of_int 1))))
			let to_float t = (float_of_int t) /. (2. ** float_of_int(FractionnalBits.bits))
      let to_string t = string_of_float (to_float t)
      let zero = 0
      let one = 1 lsl FractionnalBits.bits
      let succ t = t + 1
      let pred t = t - 1
      let min a b = min a b
      let max a b = max a b
      let gth a b = a > b
      let lth a b = a < b
      let gte a b = a >= b
      let lte a b = a <= b
      let eqp a b = a == b
      let eqs a b = a = b
      let add a b = a + b
      let sub a b = a - b
      let mul a b = (a * b) lsr FractionnalBits.bits
      let div a b = (a / b) lsl FractionnalBits.bits
      let foreach a b f =
        let rec loop a b f = match (a, b) with
          | (a, b) when a < b -> f a ; loop (succ a) b f
          | (a, _) -> f a
        in loop a b f

    end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
  print_endline "";
  let a = Fixed8.of_int 21 in
  print_string "of_int 21 = " ; print_int (Fixed8.to_int a) ; print_endline "";
  print_string "to_string 21 : " ; print_string (Fixed8.to_string a) ; print_endline "";
  print_string "zero = " ; print_string (Fixed8.to_string Fixed8.zero) ; print_endline "";
  print_string "one = " ; print_string (Fixed8.to_string Fixed8.one) ; print_endline "";
  print_string "succ of 1 = " ; print_string (Fixed8.to_string (Fixed8.succ Fixed8.one)) ; print_endline "";
  print_string "pred of 1 = " ; print_string (Fixed8.to_string (Fixed8.pred Fixed8.one)) ; print_endline "";
  let b = Fixed8.of_int 42 in
  print_string "Max of 21 | 42 = " ; print_string (Fixed8.to_string (Fixed8.max a b)) ; print_endline "";
  print_string "Min of 21 | 42 = " ; print_string (Fixed8.to_string (Fixed8.min a b)) ; print_endline "";
  let test t =
    if t then print_endline "True"
    else print_endline "False"
  in
  print_string "gth : 42 > 21 ? " ; test (Fixed8.gth b a) ; 
  print_string "gth : 21 > 42 ? " ; test (Fixed8.gth a b) ; 
  print_string "gth : 1 > 1 ? " ; test (Fixed8.gth Fixed8.one Fixed8.one) ;
  print_string "lth : 42 < 21 ? " ; test (Fixed8.lth b a) ;
  print_string "lth : 21 < 42 ? " ; test (Fixed8.lth a b) ;
  print_string "lth : 1 < 1 ? " ; test (Fixed8.lth Fixed8.one Fixed8.one) ;
  print_string "gte : 42 >= 21 ? " ; test (Fixed8.gte b a) ; 
  print_string "gte : 21 >= 42 ? " ; test (Fixed8.gte a b) ; 
  print_string "gte : 1 >= 1 ? " ; test (Fixed8.gte Fixed8.one Fixed8.one) ; 
  print_string "lte : 42 <= 21 ? " ; test (Fixed8.lte b a) ;
  print_string "lte : 21 <= 42 ? " ; test (Fixed8.lte a b) ;
  print_string "lte : 1 <= 1 ? " ; test (Fixed8.lte Fixed8.one Fixed8.one) ;
  print_string "eqp : 0 = 0 ? " ; test (Fixed8.eqp Fixed8.zero Fixed8.zero) ;
  print_string "eqp : 0 = 1 ? " ; test (Fixed8.eqp Fixed8.zero Fixed8.one) ; 
  print_string "eqs : 0 == 0 ? " ; test (Fixed8.eqs Fixed8.zero Fixed8.zero) ;
  print_string "eqs : 0 == 1 ? " ; test (Fixed8.eqs Fixed8.zero Fixed8.one) ; 
  print_string "add : 21 + 42 = "; print_string (Fixed8.to_string (Fixed8.add a b)) ; print_endline "";
  print_string "sub : 21 - 42 = "; print_string (Fixed8.to_string (Fixed8.sub a b)) ; print_endline "";
  print_string "sub : 42 - 21 = "; print_string (Fixed8.to_string (Fixed8.sub b a)) ; print_endline "";
  print_string "mul : 21 * 42 = "; print_string (Fixed8.to_string (Fixed8.mul a b)) ; print_endline "";
  print_string "div : 42 / 21 = "; print_string (Fixed8.to_string (Fixed8.div b a)) ; print_endline "";
