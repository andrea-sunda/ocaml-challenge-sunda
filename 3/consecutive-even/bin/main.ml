let consecutive_even lst =
  let rec longest_sub lst cnt m =
    match lst with
    | [] -> max cnt m
    | h :: t when h mod 2 = 0 -> longest_sub t (cnt+1) m
    | _ :: t -> longest_sub t (0) (max cnt m)
    in
  longest_sub lst 0 0
;;

(* Asserts *)
assert(consecutive_even [] = 0);;
assert(consecutive_even [1;2;3;4;5;6] = 1);;
assert(consecutive_even [1;2;2;3;4;5] = 2);;
assert(consecutive_even [1;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;3;4;2;5] = 2);;
assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;
