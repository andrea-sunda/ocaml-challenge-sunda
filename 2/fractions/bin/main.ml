let is_posfrac a = 
  if snd a = 0 then failwith("This is not a fraction")
  else
    fst a * snd a > 0;;

let compare_posfrac a b =
  if not (is_posfrac a) || not (is_posfrac b)
    then failwith("The fractions must be positive")
  else 
    let num_a = fst a * snd b in
    let num_b = snd a * fst b in
    if num_a = num_b then 0
    else if num_a > num_b then 1
    else (-1)
;;
(* some asserts *)
assert (compare_posfrac (1,2) (2,4) == 0);;
assert (compare_posfrac (1,2) (1,3) == 1);;
assert (compare_posfrac (1,2) (2,3) == -1);;

let compare_frac a b =
  match (is_posfrac a, is_posfrac b) with
  | (true, true) -> compare_posfrac (abs(fst a), abs(snd a)) (abs(fst b), abs(snd b)) 
  | (true, false) -> 1  (* Un positivo è sempre maggiore di un non-positivo *)
  | (false, true) -> -1 (* Un non-positivo è sempre minore di un positivo *)
  | (false, false) -> 
      let a_is_zero = (fst a = 0) in
      let b_is_zero = (fst b = 0) in
      if a_is_zero && b_is_zero then 0 (* 0 == 0 *)
      else if a_is_zero then 1 (* 0 > negative number *)
      else if b_is_zero then -1 (* negative number < 0 *)
      else
        compare_posfrac (abs(fst b), abs(snd b)) (abs(fst a), abs(snd a)) (* compares two negative numbers *)
;;
(* some asserts *)
assert (compare_frac (-1,2) (2,4) = -1);;
assert (compare_frac (1,2) (1,-3) = 1);;
assert (compare_frac (1,-2) (-2,3) = 1);;
