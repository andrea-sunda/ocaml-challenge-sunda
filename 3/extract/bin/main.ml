let rec extract n = function
  | [] -> failwith "index out of bounds"
  | h :: t ->
      if n = 0 then (h, t)
      else
        let (x, t') = extract (n - 1) t in
        (x, h :: t')
;;

(* asserts *)
assert(extract 0 [1;2;3] = (1, [2; 3]));;
assert(extract 1 [1;2;3] = (2, [1; 3]));;
assert(extract 2 [1;2;3] = (3, [1; 2]));;
