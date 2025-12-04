let countzero f a b =
  let rec loop n cnt =
    match n with
    | x when x >= b -> cnt
    | x when f x = 0 -> loop (x+1) (cnt+1)
    | x -> loop (x+1) (cnt)
  in
  loop a 0
;;

assert (countzero (fun x -> x) (-10) 10 = 1);;
assert (countzero (fun x -> x) 1 10 = 0);;
assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);;
assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;
