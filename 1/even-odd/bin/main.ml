(* is_even function *)
let is_even (num: int) : bool =
  (num mod 2) == 0;;

(* solution with if-else construct *)
let win (a : int) (b : int) : int =
  let a_valid = a >= 1 && a <= 5 in
  let b_valid = b >= 1 && b <= 5 in
  if not a_valid
    then
      if not b_valid
        then 0
      else -1
  else
    if not b_valid
      then 1
    else
      if is_even (a+b)
        then 1
      else -1
;;

(* solution with pattern matching *)
let win (a : int) (b : int) : int =
  let a_valid = a >= 1 && a <= 5 in
  let b_valid = b >= 1 && b <= 5 in
  match (a_valid,b_valid) with
  | (false,false) -> 0
  | (true,false) -> 1
  | (false,true) -> -1
  | (true,true) -> if is_even (a+b) then 1 else -1
;;
