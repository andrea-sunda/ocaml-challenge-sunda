
type season = Spring | Summer | Autumn | Winter;;

(* First solution *)
let squirrel_play (a : int) (b : season) : bool =
  match (a,b) with
  | (d,Summer) when d >= 15 && d <= 35 -> true
  | (d,_) when d >= 15 && d <= 30 -> true
  | _ -> false
;;

(* Second solution *)
let squirrel_play (a : int) (b : season) : bool =
  let lower_limit = 15 in
  let upper_limit =
    match b with
    | Summer -> 35
    | _ -> 30
  in
  a >= lower_limit && a <= upper_limit
;;
