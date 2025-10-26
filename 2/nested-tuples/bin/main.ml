(* function to calculate the sum of scores *)
let total_score (s : (string * (int * int))) =
  let scores = snd s in
  fst scores + snd scores
;;

(* function to verify that both written and oral are at minimum 18 *)
let passed (s : (string * (int * int))) =
  let scores = snd s in
  fst scores >= 18 && snd scores >= 18
;;

(* function to return a string that explain the result of an exam *)
let string_of_result (s : string * (int * int)) =
  let name = fst s in
  let scores = snd s in
  if passed s
  then
    let sum = (fst scores) + (snd scores) in
    name ^ " passed with total " ^ (string_of_int sum)
  else
    name ^ " did not pass"
;;

(* function to merge grades of the same student *)
let merge (s0 : string * (int * int)) (s1 : string * (int * int)) =
  if (fst s0) <> (fst s1)
    then failwith("The grades are not from the same student!")
  else
    let s0_grades = snd s0 in
    let s1_grades = snd s1 in
    (fst s0, ((max (fst s0_grades) (fst s1_grades)), (max (snd s0_grades) (snd s1_grades))))
;;

(* some asserts *)
assert(total_score ("Andrea", (10, 20)) = 30);;
assert(passed ("Andrea", (10, 20)) = false);;
assert(passed ("Andrea", (20, 20)) = true);;
assert(string_of_result ("Andrea", (20, 20)) = "Andrea passed with total 40");;
assert(string_of_result ("Andrea", (10, 20)) = "Andrea did not pass");;
assert ((merge ("Andrea", (10, 20)) ("Andrea", (20, 10))) = ("Andrea", (20, 20)));;
