let rec is_perforated lst =
  let perforated a b =
    abs(a - b) > 1
  in
  match lst with
  | [] -> true
  | [_] -> true
  | h :: b :: t -> 
    if (perforated h b) then is_perforated (b :: t)
    else false
;;

(* asserts *)
assert(is_perforated []);;
assert(is_perforated [1]);;
assert(is_perforated [1;2] = false);;
assert(is_perforated [1;3]);;
assert(is_perforated [1;5;2]);;
assert(is_perforated [1;3;2] = false);;
assert(is_perforated [1;4;2;0]);;
assert(is_perforated [1;3;2;0] = false);;
assert(is_perforated [1;3;5;2;4;7;3;1]);;