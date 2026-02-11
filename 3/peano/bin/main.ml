type nat = Z | S of nat;;

(* detects if a number is even *)
let rec is_even = function
  | Z -> true
  | S Z -> false
  | S(S x) -> is_even x
;;

(* divides a natural number by two *)
let rec halve = function
  | Z -> Z
  | S Z -> Z
  | S(S x) -> S (halve x)
;;

(* adds two numbers *)
let rec add p1 = function 
  | Z -> p1
  | S(x) -> S (add p1 x)
;;

(* multiplies two numbers *)
let rec mul p1 p2 =
  match p1 with
  | Z -> Z
  | S n -> add p2 (mul n p2)
;;

(* checks equality *)
let rec equals p1 p2 =
  match (p1, p2) with
  | (Z, Z) -> true
  | (Z, _) -> false
  | (_, Z) -> false
  | (S x1, S x2) -> equals x1 x2
;;

(* checks if a number is less than or equal to another one *)
let rec leq p1 p2 =
  match (p1, p2) with
  | (Z, _) -> true
  | (_, Z) -> false
  | (S x1, S x2) -> leq x1 x2
;;
