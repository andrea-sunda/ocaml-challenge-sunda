(* First part *)
let apply lst key =
  match (List.filter (function (x,y) -> x=key) lst) with
  | [] -> None
  | (_,y) :: _ -> Some y
;;

(* Asserts *)
let f0 = [(1, 7); (2, 3); (4, 5); (5, 6); (7, 9); (2, 4); (8, 3)];;
assert(apply f0 4 = Some 5);;
assert(apply f0 6 = None);;
assert(apply f0 2 = Some 3);;

(* Second part *)
let mkfun lst =
  match lst with
  | [] -> []
  | (h1, h2) :: t ->
  [(h1,h2)] @ (List.filter (function (x, y) -> x <> h1) t)
;;

(* Asserts *)
assert(mkfun [(1,7);(2,3)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(1,3)] = [(1,7)]);;
assert(mkfun [(1,7);(2,3);(1,5)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8);(2,4)] = [(1,7);(2,3)]);;
