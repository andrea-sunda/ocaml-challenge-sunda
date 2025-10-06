(* First implementation of mux2 *)
let mux2 s0 a b = (s0 && a) || (not s0 && b);;

(* Second implementation of mux2 *)
let mux2 s0 (a:bool) (b:bool) = if s0 then a else b;;

(* Third implementation of mux2 *)
let mux2 s0 (a : bool) (b : bool) = match(s0) with
(true) -> a
| (false) -> b;;
