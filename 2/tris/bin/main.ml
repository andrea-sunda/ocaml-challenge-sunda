(* operator that checks if the three values are equals *)
let ( === ) a b c = 
  a = b && a = c 
;;

let print_tuple tuple =
  match tuple with
  | (a, b, c, d) ->
    print_char '(';
    print_int a;
    print_char ',';
    print_int b;
    print_char ',';
    print_int c;
    print_char ',';
    print_int d;
    print_string ")\n"
  ;;

let tris (a, b, c, d) =
  ( === ) b c d || ( === ) a c d || ( === ) a b d || ( === ) a b c
;;

let hand () = (Random.int 10 + 1, Random.int 10 + 1, Random.int 10 + 1, Random.int 10 + 1);;

(* test *)
let test =
  let test_hand = hand() in
  print_tuple test_hand;
  tris test_hand
;;

