type 'a option = Some of 'a | None

let minmaxfun f a b =
  if a > b then None
  else
    let rec min_max n minimum maximum =
      if n > b then Some(minimum, maximum)
      else
        let y = f n in
        match (y < minimum, y > maximum, y) with
        | (true, _, x) -> min_max (n+1) x maximum
        | (_, true, x) -> min_max (n+1) minimum x
        | _ -> min_max (n+1) minimum maximum
      in 
      min_max (a+1) (f a) (f a)
;;

assert (minmaxfun (fun x -> x) (-2) 5 = Some (-2,5));;
assert (minmaxfun (fun x -> x) 5 (-2) = None);;
assert (minmaxfun (fun x -> x) 5 5 = Some (5,5));;
assert (minmaxfun (fun x -> x * x) (-2) 5 = Some (0,25));;

let curve x = x |> Float.of_int |> fun x -> x ** 3.0 -. 3.0 *. x;;
let arccos x = x |> Float.of_int |> Float.acos;;

assert (minmaxfun curve (-2) 2 = Some (-2.0,2.0));;
assert (minmaxfun arccos (-1) 1 = Some (0., Float.pi));;
