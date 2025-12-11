let string_of_list lst =
  let rec append = function
  | [] -> ""
  | [h] -> string_of_int h
  | h :: t -> (string_of_int h) ^ ";" ^ (append t)
  in
  "[" ^ append lst ^ "]"
;;
  
assert(string_of_list [1;2;3] = "[1;2;3]");;
assert(string_of_list [] = "[]");;
