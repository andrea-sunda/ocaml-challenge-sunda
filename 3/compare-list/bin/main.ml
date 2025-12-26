let rec compare_list comp lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | (h1 :: t1, h2 :: t2) ->
    match comp h1 h2 with
    | 0 -> compare_list comp t1 t2
    | result -> result
;;

(* Asserts  *)
assert (compare_list Int.compare [0] [1] <= 0);;
assert (compare_list Int.compare [0;1;0;0] [0;1;0;0] = 0);;
assert (compare_list Int.compare [0;1;0;0] [0;0;1;0] >= 0);;
assert (compare_list Int.compare [1;1;1;1] [1] >= 0);;
assert (compare_list Int.compare [1;1;2;3] [1;2;3] <= 0);;
assert (compare_list Int.compare [2;42;1] [3] <= 0);;
assert (compare_list Char.compare ['a';'b';'a';'c';'o'] ['a';'b';'e';'t';'e'] <= 0);;
