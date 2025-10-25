(* Operation variant *)
type operation = Add of int * int | Sub of int * int | Mul of int * int | Div of int * int;;

(* function to evaluate an operation *)
let eval (op) =
  match op with
  | Add(a,b) -> Some(a + b)
  | Sub(a,b) -> Some(a - b)
  | Mul(a,b) -> Some(a * b)
  | Div(a,b) -> if b != 0 then Some(a / b) else None
;;

(* function to create an operation *)
let make_operation op a b =
  match op with
  | '+' -> Add(a,b)
  | '-' -> Sub(a,b)
  | '*' -> Mul(a,b)
  | '/' when b != 0 -> Div(a,b)
  | _ -> failwith("Invalid operation")
;;

(* some asserts *)
assert(eval @@ Add(2,3) = Some(5));;
assert(eval @@ Sub(2,3) = Some(-1));;
assert(eval @@ Mul(2,3) = Some(6));;
assert(eval @@ Div(10,2) = Some(5));;
assert(eval @@ Div(10,0) = None);;
assert((make_operation '+' 2 3) = Add(2,3));;
assert((make_operation '-' 2 3) = Sub(2,3));;
assert((make_operation '*' 2 3) = Mul(2,3));;
assert((make_operation '/' 2 3) = Div(2,3));;
