let rec rotate n lst =
  match lst with
  | [] -> lst
  | _ when n <= 0 -> lst
  | h :: t -> rotate (n-1) (t @ [h])
;;

assert(rotate 0 [5;6;7;8] = [5;6;7;8]);;
assert(rotate 1 [5;6;7;8] = [6;7;8;5]);;
assert(rotate 2 [5;6;7;8] = [7;8;5;6]);;
assert(rotate 3 [5;6;7;8] = [8;5;6;7]);;
assert(rotate 4 [5;6;7;8] = [5;6;7;8]);;
