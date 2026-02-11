let enum_nat_nat n =
  let rec trova_diagonale d resta =
    if resta <= d then 
      (resta, d - resta)
    else 
      trova_diagonale (d + 1) (resta - (d + 1))
  in
  trova_diagonale 0 n
;;
