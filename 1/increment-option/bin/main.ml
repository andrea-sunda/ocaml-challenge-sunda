(* Solution with pattern matching *)
let incr_opt (x : int option) =
  match x with
  | None -> None
  | Some x -> Some(x+1)
;;
