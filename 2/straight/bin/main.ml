type suit = S | H | D | C;;
type card = Card of int * suit;;

let straight (Card(val1, _), Card(val2, _), Card(val3, _), Card(val4, _), Card(val5, _)) =
  val1 = val2-1 && val2 = val3-1 && val3 = val4-1 && val4 = val5-1
;;
(* some asserts *)
assert(straight (Card(1, H), Card(2, D), Card(3, C), Card(4, H), Card(5, S)) = true);;
assert(straight (Card(1, H), Card(2, D), Card(3, C), Card(2, H), Card(5, S)) = false);;
