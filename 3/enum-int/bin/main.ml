let enum_int = function
  | n when n mod 2 = 0 -> n/2
  | n -> -(n/2+1)
;;

assert (List.init 10 enum_int = [0; -1; 1; -2; 2; -3; 3; -4; 4; -5]);;
