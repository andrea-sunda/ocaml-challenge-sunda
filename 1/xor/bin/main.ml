(* First implementation *)
let xor1 a b = ((a && b) || (not a && not b));;

(* Second implementation *)
let xor2 a b = if (a == b) then false else true;;

(* Third implementation *)
let xor3 a b = match(a,b) with
    (true,true) -> false
| (true,false) -> true
| (false,true) -> true
| (false,false) -> false;;
