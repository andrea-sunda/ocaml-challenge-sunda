let bounce n =
  fun x -> n - abs (n - (x mod (2*n)))
;;
