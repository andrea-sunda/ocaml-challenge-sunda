let rec l0 = function
  | [1] -> true
  | 0 :: t -> l0 t
  | 1 :: t -> l0 t
  | _ -> false
;;

let l1 lst =
  let rec no_zero = function
    | [] -> true
    | 0 :: _ -> false
    | 1 :: t ->  no_zero t
    | _ -> false
  in
  let rec f = function
    | [] -> true
    | 1 :: t -> f t
    | 0 :: t -> no_zero t 
    | _ -> false
  in
  f lst
;;

let l2 lst =
  let rec no_zero = function
    | [] -> true
    | 0 :: _ -> false
    | (1 | 2) :: t ->  no_zero t
    | _ -> false
  in
  let rec f = function
    | [] -> true
    | 1 :: t -> no_zero t
    | (0 | 2) :: t -> f t
    | _ -> false
  in
  f lst
;;

let rec l3 = function
  | [] -> true
  | 0 :: 1 :: 1 :: t -> l3 t
  | 0 :: _ -> false
  | _ :: t -> l3 t
;;

let l4 lst =
  let rec count_zeros_ones zeros ones = function
    | [] -> zeros >= ones
    | 0 :: t -> count_zeros_ones (zeros+1) ones t
    | 1 :: t -> count_zeros_ones zeros (ones+1) t
    | _ -> false
  in
  count_zeros_ones 0 0 lst
;;

let l5 lst =
  let rec count_zeros_ones zeros ones = function
    | [] -> zeros = ones
    | 0 :: t -> count_zeros_ones (zeros+1) ones t
    | 1 :: t -> count_zeros_ones zeros (ones+1) t
    | _ -> false
  in
  count_zeros_ones 0 0 lst
;;

let l6 lst =
  let rec ones n = function
    | [] -> n = 0
    | 1 :: t -> ones (n-1) t
    | _ -> false
  in
  let rec zeros n = function
    | [] -> n = 0
    | 0 :: t -> zeros (n+1) t
    | 1 :: t -> ones (n-1) t
    | _ -> false  
  in
  zeros 0 lst
;;

let l7 lst =
    let rec snd_zeros n = function
      | [] -> n = 0
      | 0 :: t -> snd_zeros (n-1) t
      | _ -> false
    in
    let rec zeros n = function
      | 0 :: t -> zeros (n+1) t
      | 1 :: t -> snd_zeros n t
      | _ -> false  
  in
  zeros 0 lst
;;

let l8 lst =
  let rec count_zeros_twos cnt = function
    | [] -> cnt = 0
    | 0 ::  t -> count_zeros_twos (cnt+1) t
    | 2 :: t -> count_zeros_twos (cnt-1) t
    | 1 :: t -> count_zeros_twos cnt t
    | _ -> false
  in
  let rec first_one = function
    | [] -> false
    | 1 :: t -> count_zeros_twos 0 t
    | (0 | 2) :: t -> first_one t
    | _ -> false
  in
  first_one lst
;;
