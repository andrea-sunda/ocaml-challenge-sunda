let consensus3 (f0, f1, f2)n =
  match (f0 n, f1 n, f2 n) with
    | (a, b, c) when (a = b) -> Some(a)
    | (a, b, c) when (a = c) -> Some(a)
    | (a, b, c) when (b = c) -> Some(b)
    | _ -> None
;;

assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 1 = Some 5);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 2 = Some 2);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 3 = None);;
assert (
  try
    consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 0
    |> ignore; false
  with _ -> true);;