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
  let result = 
    List.fold_left (fun acc -> fun n ->
      if List.exists (fun (x,y) -> x = (fst n)) acc then acc
      else n :: acc) [] lst
    in
  List.rev result
;;

(* Asserts *)
assert(mkfun [(1,7);(2,3)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(1,3)] = [(1,7)]);;
assert(mkfun [(1,7);(2,3);(1,5)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8)] = [(1,7);(2,3)]);;
assert(mkfun [(1,7);(2,3);(1,5);(1,8);(2,4)] = [(1,7);(2,3)]);;
