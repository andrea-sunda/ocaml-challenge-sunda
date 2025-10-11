type blood_group = A | B | AB | O;;

(* Solution with if condition *)
let check_groups1 (x : blood_group) (y : blood_group) =
  if x = y || x = O || (y = AB && (x = A || x = B)) then true else false;;

(* Solution with pattern matching *)
let check_groups2 (x : blood_group) (y : blood_group) =
  match(x,y) with
  (O,_) -> true
  | (_,AB) -> true
  | (A,A) -> true
  | (B,B) -> true
  | _ -> false;;

(* More elegant solution with pattern matching)
let check_groups (x : blood_group) (y : blood_group) : bool =
  match x with
  | O -> true
  | A -> (y = A || y = AB)
  | B -> (y = B || y = AB)
  | AB -> (y = AB);;
