let knife lst n =
  let rec split lst1 lst2 cnt =
    match lst1 with
    | [] -> (lst1, lst2)
    | h :: t when (cnt = n-1) -> (lst2 @ [h], t)
    | h :: t -> split t (lst2 @ [h]) (cnt+1)
  in
  split lst [] 0
;;

(* asserts *)
assert (knife [1;2;3;4;5;6] 3 = ([1;2;3], [4;5;6]));;
assert (knife ['b';'r';'e';'a';'d'] 3  = (['b';'r';'e'], ['a';'d']));;
assert (knife [] 0 = ([], []));;
assert (knife ["miss"; "me"] 2  = (["miss"; "me"], []));;
assert (knife ["oops"] (-1)  = ([], ["oops"]));;
