(* mem x s is true iff the element x belongs to the set s *)
let rec mem x = function
  | [] -> false
  | h :: t ->
    if h = x then true
    else mem x t
;;

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;

(* subseteq xl yl is true iff xl is a subset of yl *)
let rec subseteq set1 set2 =
  match set1 with
  | [] -> true
  | h :: t -> 
      if mem h set2 then subseteq t set2 
      else false
;;

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;

(* seteq xl yl is true iff xl is a equal to yl up-to reordering of elements *)
let seteq lst1 lst2 =
  (subseteq lst1 lst2) && (subseteq lst2 lst1)
;;

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
(** dup l evaluates to true iff the list l contains duplicates (and therefore, it is not a set) *)
let rec dup = function
  | [] -> false
  | h :: t ->
    if mem h t then true
    else dup t
;;

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;

(* mkset l removes the duplicates from the list l, making is a set *)
let rec mkset = function
  | [] -> []
  | h :: t ->
    if mem h t then mkset t
    else h :: mkset t
;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;

(* The following functions compute the union, intersection and difference of two sets *)
let union lst1 lst2 =
  mkset (lst1 @ lst2)
;;

let rec inter lst1 = function
  | [] -> []
  | h :: t -> 
    if mem h lst1 then h :: inter lst1 t
    else inter lst1 t
;;

let rec diff lst1 lst2 =
  match lst1 with
  | [] -> []
  | h :: t -> 
      if mem h lst2 then diff t lst2
      else h :: diff t lst2
;;

assert(seteq (union [1;2;3] []) [1;2;3]);;
assert(seteq (union [] [2;3;4]) [2;3;4]);;
assert(seteq (union [1;2;3] [2;3;4]) [1;2;3;4]);;
assert(seteq (inter [1;2;3] []) []);;
assert(seteq (inter [] [2;3;4]) []);;
assert(seteq (inter [1;2;3] [2;3;4]) [2;3]);;
assert(seteq (diff [1;2;3] []) [1;2;3]);;
assert(seteq (diff [] [2;3;4]) []);;
assert(seteq (diff [1;2;3] [2;3;4]) [1]);;
assert(seteq (diff [1;2;3] [3;1]) [2]);;

(* dsum xl yl computes the disjoint union between xl and yl *)
let dsum lst1 lst2 =
  let rec mktuple n = function
    | [] -> []
    | h :: t -> (n, h) :: mktuple n t
  in
  mktuple 0 lst1 @ mktuple 1 lst2
;;

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;

(* powset xl computes the set of all subsets of xl *)
let rec powset = function
  | [] -> [[]]
  | h :: t ->
      let rest = powset t in
      let with_h = List.map (fun subset -> h :: subset) rest in
      rest @ with_h
;;
assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;
