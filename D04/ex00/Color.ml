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