type suit = S | H | D | C;;
type card = Card of int * suit;;

let rnd_suit() =
  match (Random.int 4) with
  | (0) -> S
  | (1) -> H
  | (2) -> D
  | _ -> C
;;

let rndCard() : card =
  Card((Random.int 10)+1, rnd_suit())
;;

let rndHand() =
  (rndCard(), rndCard(), rndCard(), rndCard(), rndCard())
;;

let couple (Card (val1, suit1)) (Card (val2, suit2)) = 
  (val1  = val2) && (suit1 <> suit2)
;;

let poker_four_card (a, b, c, d) =
  couple a b && couple a c && couple a d && couple b c && couple b d && couple c d
;;

let poker (a, b, c, d, e) =
  poker_four_card (b, c, d, e) 
  || poker_four_card (a, c, d, e) 
  || poker_four_card (a, b, d, e) 
  || poker_four_card (a, b, c, e) 
  || poker_four_card (a, b, c, d)
;;
