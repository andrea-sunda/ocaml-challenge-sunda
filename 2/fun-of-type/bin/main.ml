(* f1 : int -> bool *)
let f1 x = x>0;;
(* f2 : bool -> int *)
let f2 x = if x then 1 else 0;;
(* f3 : int -> (int * bool) *)
let f3 x = (x,true);;
(* f4 : (int * bool) -> int *)
let f4 x = if snd x then fst x + 1 else fst x + 2;;
(* f5 : int -> (int -> int) *)
let f5 x y = x+y;;
(* f6 : int -> (int -> bool) *)
let f6 x y = x+y > 0;;
(* f7 : bool -> (int -> bool) *)
let f7 x y = if x then y+1 > 0 else y+1 < 0;;
(* f8 : bool -> (bool -> int) *)
let f8 x y = if x && y then 0 else 1;;
(* f9 : bool -> (int -> int) *)
let f9 x y = if x then y+1 else y+2;;
(* f10 : (int -> int) -> int *)
let f10 f = (f 2) + 2 ;;
(* f11 : (int -> bool) -> int *)
let f11 f = if f 1 then 1 else 0;;
(* f12 : (bool -> int) -> int *)
let f12 f = if f true = 1 then 1 else 0;;
(* f13 : (int -> bool) -> bool *)
let f13 f = f 1 = true;;
(* f14 : (bool -> bool) -> int *)
let f14 f = if f true then 1 else 0;;
(* f15 : int -> (int * int) -> int *)
let f15 x y = x + fst y + snd y;;
(* f16 : int -> (int -> (int -> int)) *)
let f16 = fun x -> fun y -> fun z -> x + y + z;;
(* f17 : (int -> int) -> (int -> int) *)
let f17 f = fun x -> f (x+1)+1;;
(* f18 : ((int -> int) -> int) -> int *)
let f18 f = f (fun x -> x+1)+1;;
(* f19 : (int -> int) -> (bool -> bool) *)
let f19 f x = f 1 = 2 && x;;
(* f20 : (int -> bool) -> (bool -> int) *)
let f20 f x = if (f 1 && x) then 1 else 0;;
