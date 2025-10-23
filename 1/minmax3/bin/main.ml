let minmax3 a b c =
  let minimum = min (min a b) c in
  let maximum = max (max a b) c in
  (minimum,maximum)
;;
