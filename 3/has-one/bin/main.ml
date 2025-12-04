(* first implementation *)
let has_one n =
  if n < 0 then failwith "The number must be positive!" else
  let rec find_1 pow_10 =
    if n < pow_10 then (n mod pow_10 / (pow_10 / 10) = 1)
    else if n mod pow_10 / (pow_10 / 10) = 1
      then true
    else
        find_1 (pow_10 * 10)
    in 
    find_1 10
;;

(* second implementation with pattern matching *)
let rec has_one n =
  match n with
  | x when x < 0 -> failwith "The number must be positive!"
  | 0 -> false
  | x when x mod 10 = 1 -> true
  | x -> has_one (x / 10)
;;

assert(has_one 10 = true);;
assert(has_one 220 = false);;
assert(has_one 911 = true);;
assert(has_one 451 = true);;
assert(try has_one (-1) |> fun _ -> false with _ -> true);;
