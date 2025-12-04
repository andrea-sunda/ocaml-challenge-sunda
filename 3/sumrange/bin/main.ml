let rec sumrange a b =
  if a > b then 0
  else
    sumrange a (b-1) + b
;;

assert (sumrange 0 1 = 1);;
assert (sumrange 1 3 = 6);;
assert (sumrange 3 2 = 0);;
