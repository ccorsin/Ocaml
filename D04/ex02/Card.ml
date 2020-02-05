module Color =
  struct
    type t = Spade | Heart | Diamond | Club

    let all = [Spade ; Heart ; Diamond ; Club]

    let toString (t:t) = match t with
      | Spade -> "S"
      | Heart -> "H"
      | Diamond -> "D"
      | Club -> "C"

    let toStringVerbose (t:t) = match t with
      | Spade -> "Spade"
      | Heart -> "Heart"
      | Diamond -> "Diamond"
      | Club -> "Club"
  end

module Value =
  struct
    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]
    
    let toInt (c:t) = match c with
      | T2 -> 1
      | T3 -> 2
      | T4 -> 3
      | T5 -> 4
      | T6 -> 5
      | T7 -> 6
      | T8 -> 7
      | T9 -> 8
      | T10 -> 9
      | Jack -> 10
      | Queen -> 11
      | King -> 12
      | As -> 13
    
    let toString (t:t) = match t with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | As -> "A"
    
    let toStringVerbose (t:t) = match t with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen -> "Queen"
    | King -> "King"
    | As -> "As"
    
    let next (t:t) : t = match t with
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen -> King
    | King -> As
    | As -> invalid_arg "No next value"
    
    let previous (t:t) : t = match t with
    | T2 -> invalid_arg "No previous value"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen -> Jack
    | King -> Queen
    | As -> As
  end

  type t = {
    color: Color.t;
    value: Value.t
  }

  let newCard (v:Value.t) (c:Color.t) :t =
    {color = c; value = v}

  let allSpades =
    let rec loop_in_all l = match l with
      | [] -> []
      | (h:Value.t)::q -> ({color = Color.Spade; value = h})::(loop_in_all q)
    in
    loop_in_all Value.all

  let allHearts =
    let rec loop_in_all l = match l with
      | [] -> []
      | (h:Value.t)::q -> ({color = Color.Heart; value = h})::(loop_in_all q)
    in
    loop_in_all Value.all

  let allDiamonds =
    let rec loop_in_all l = match l with
      | [] -> []
      | (h:Value.t)::q -> ({color = Color.Diamond; value = h})::(loop_in_all q)
    in
    loop_in_all Value.all

  let allClubs =
    let rec loop_in_all l = match l with
      | [] -> []
      | (h:Value.t)::q -> ({color = Color.Club; value = h})::(loop_in_all q)
    in
    loop_in_all Value.all

  let all =
    allSpades@allHearts@allDiamonds@allClubs

  let getValue  self =
    self.value

  let getColor self =
    self.color

  let toString self =
    (Value.toString self.value) ^ (Color.toString self.color)

  let toStringVerbose self =
    "Card(" ^ (Value.toStringVerbose self.value) ^ ", "  ^ (Color.toStringVerbose self.color) ^ ")"

  let compare t1 t2 =
    (Value.toInt t1.value) - (Value.toInt t2.value)

  let max t1 t2 =
    if (Value.toInt t1.value) >= (Value.toInt t2.value) then
      t1
    else t2

  let min t1 t2 =
    if (Value.toInt t1.value) <= (Value.toInt t2.value) then
      t1
    else t2
  
  let best l = match l with
    | [] -> invalid_arg "Empty list"
    | h::t -> List.fold_left max h t
  
  let isOf (t:t) (color:Color.t) =
    (t.color = color)

  let isSpade (t:t) = (t.color = Color.Spade)

  let isHeart (t:t) = (t.color = Color.Heart)

  let isDiamond (t:t) = (t.color = Color.Diamond)

  let isClub (t:t) = (t.color = Color.Club)