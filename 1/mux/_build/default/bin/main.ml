(* First implementation of mux2 *)
let mux2_1 s0 a b = (s0 && a) || (not s0 && b);;

(* Second implementation of mux2 *)
let mux2_2 s0 (a:bool) (b:bool) = if s0 then a else b;;

(* Third implementation of mux2 *)
let mux2_3 s0 (a : bool) (b : bool) = match(s0) with
(true) -> a
| (false) -> b;;

(* First implementation of mux4 *)
let mux4_1 s1 s0 a0 a1 a2 a3 = (not s1 && not s0 && a0) || (not s1 && s0 && a1) || (s1 && not s0 && a2) || (s1 && s0 && a3);;

(* Second implementation of mux4 *)
let mux4_2 s1 s0 a0 a1 a2 a3 = if (mux2_1 s1 true false) then (mux2_2 s0 a2 a3) else (mux2_3 s0 a0 a1);;

(* Third implementation of mux4 *)
let mux4_3 s1 s0 (a0 : bool) (a1 : bool) (a2 : bool) (a3 : bool) = match(s1,s0) with
(false,false) -> a0
| (false,true) -> a1
| (true,false) -> a2
| (true,true) -> a3;;
