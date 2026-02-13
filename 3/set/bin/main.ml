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

let rec mkset = function
  | [] -> []
  | h :: t ->
    if mem h t then mkset t
    else h :: mkset t
;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;
