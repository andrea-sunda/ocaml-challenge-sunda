let first_third_fifth lst =
  match lst with
  | a :: _ :: b:: _ :: c :: _ -> Some (a, b, c)
  | _ -> None
;;

assert(first_third_fifth ["cat"; "dog"] = None);;
assert(first_third_fifth [1; 2; 3; 4; 5; 6] = Some (1, 3, 5));;
