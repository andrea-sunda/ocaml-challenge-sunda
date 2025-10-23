type card = Joker | Val of int;;

(* Solution with pattern matching *)
let win player_card dealer_card =
  match (player_card,dealer_card) with
  | (_,Joker) -> false
  | (Joker,_) -> true
  | (_,_) -> player_card > dealer_card
;;
