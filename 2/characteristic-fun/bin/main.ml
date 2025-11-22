(* ∅ *)
let f1 x = if x < 0 then failwith("The number is not a natural number") else 0;;
(* {0, 1, 2} *)
let f2 x =
  match x with
  | a when a >= 0 && a <= 2 -> 1
  | a  when a < 0 -> failwith("The number is not a natural number")
  | _ -> 0
;;
(* {x | 0 < x < 100} *)
let f3 x =
  match x with
  | a when a > 0 && a < 100 -> 1
  | a when a < 0 -> failwith("The number is not a natural number")
  | _ -> 0
;;
(* {x | false } *)
let f4 x = 
  if x < 0 then 
    failwith("The number is not a natural number") 
else 0;;
(* {x | ∀y. x + y ≥ x} *)
let f5 x = 
  if x < 0 then 
    failwith("The number is not a natural number") 
  else 1;;
(* {x | ∃y. x = y + y} *)
let f6 x = (x+1) mod 2;;
(* {x | ∃y. y = x + x} *)
let f7 x = 
  if x < 0 then 
    failwith("The number is not a natural number") 
  else 
    (x+1) mod 2
;;
(* {x | ∃y. x < y} *)
let f8 x =
  if x < 0 then
    failwith("The number is not a natural number")
  else 1
;;
(* {x | ∃y. x * x = y} *)
let f9 x =
  if x < 0 then
    failwith("The number is not a natural number")
  else 1
;;
(* {x | ∃y. y < 3 ∧ 7 < x y < 20 } *)
let f10 x =
  match x with
  | a when a >= 4 && a <= 19 -> 1
  | a when a < 0 -> failwith("The number is not a natural number")
  | _ -> 0
;;
(* {x | x < 50 ∧ ∃y. x = y + y} *)
let f11 x =
  match x with
  | a when a < 0 -> failwith("The number is not a natural number")
  | a when a < 50 && a mod 2 = 0 -> 1
  | _ -> 0
;;
(* {z | ∃x. z = 2 x ∧ 0 < 2x < 50 } *)
let f12 x =
    match x with
  | a when a < 0 -> failwith("The number is not a natural number")
  | a when a >= 2 && a < 25 && a mod 2 = 0  -> 1
  | _ -> 0
;;
(* {z | ∃x, y. z = x + y ∧ x * y ≤ 20} *)
let f13 z =
  match z with
  | a when a < 0 -> failwith("The number is not a natural number")
  | a when a-1 <= 20 -> 1
  | _ -> 0
;;
(* {x | ∀y. y < x → y < 2 } *) 
let f14 x =
  match x with
  | a when a < 0 -> failwith("The number is not a natural number")
  | a when a <= 2-> 1
  | _ -> 0
;;
(* {x | ∀y. (x mod y = 0) → (y=1 ∨ y=x) } *)
let f15 x =
  if x < 0 then failwith("The number is not a natural number")
  else if x <= 1 then 0
  else if x = 2 then 1
  else if x mod 2 = 0 then 0
  else
    let limit = int_of_float (sqrt (float_of_int x)) in
    let rec find_divisors y z =
      if z > limit then 1
      else if y mod z = 0
        then 0
      else find_divisors y (z+2)
     in
  find_divisors x 3
;;
