(* square function *)
let square x = x*x;;

(* exp9 function with less multiplications possibile *)
let exp9 x = square (square (square x)) * x;;

