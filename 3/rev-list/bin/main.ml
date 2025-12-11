let rec rev lst =
  match lst with
  | [] -> lst
  | h :: t -> rev(t) @ [h]
;;

assert(rev [1;2;3] = [3;2;1]);;
