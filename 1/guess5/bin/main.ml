let guess5 n =
  if n < 1 || n > 5 then failwith("Number out of bounds[1,5]")
  else
    let r = Random.int(5) + 1 in
    let correct = n = r in
    (correct,r)
;;
