let best_offer (g0 : int option) (g1 : int option) (g2 : int option) : int option =
  let max g0 g1 : int option =
    match (g0,g1) with
      | (None,_) -> g1
      | (_,None) -> g0
      | (Some x, Some y) -> if x > y then (Some x) else (Some y)
  in
  max (max g0 g1) g2
;;
