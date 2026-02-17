let knife lst n =
  let rec split lst1 lst2 cnt =
    match lst1 with
    | [] -> (lst1, lst2)
    | h :: t when (cnt = n-1) -> (lst2 @ [h], t)
    | h :: t -> split t (lst2 @ [h]) (cnt+1)
  in
  split lst [] 0
;;

let rec merge lst1 lst2 =
  match (lst1, lst2) with
  | ([], []) -> []
  | (lst1, []) -> lst1
  | ([], lst2) -> lst2
  | (h1 :: t1, h2 :: t2) ->
    if h1 <= h2 then h1 :: merge t1 (h2 :: t2)
    else h2 :: merge (h1 :: t1) t2
;;

let halve lst =
  knife lst ((List.length lst)/2)
;;

let rec merge_sort = function
  | [] -> []
  | [a] -> [a]
  | lst -> 
      let (l, r) = halve lst in
      merge (merge_sort l) (merge_sort r)
;;

(* asserts *)
assert (merge [1;4;5] [2;3;6] = [1;2;3;4;5;6]);;
assert (merge [7] [2;3;6] = [2;3;6;7]);;
assert (merge [7] [] = [7]);;
assert (halve [5;1;3;8;-2;6] = ([5;1;3], [8;-2;6]));;
assert (halve [1;3] = ([1], [3]));;
assert (halve [5;1;3] = ([5], [1;3]));;
assert (merge_sort [5;1;3;8;-2;6] = [-2;1;3;5;6;8]);;
