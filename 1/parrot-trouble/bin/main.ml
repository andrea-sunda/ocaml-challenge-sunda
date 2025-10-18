(* Solution with if-else *)
let parrot_trouble (a : bool) (b : int) : bool option =
  if b < 0 || b > 23 then None else Some (a && (b < 7 || b > 20));;

(* Solution with pattern matching *)
let parrot_trouble (a : bool) (b : int) : bool option =
  match (a,b) with
  | (_,h) when h < 0 || h > 23 -> None
  | (true,h) when h < 7 || h > 20 -> Some true
  | _ -> Some false
;;
