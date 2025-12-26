(* Recursive solution  *)
let rnd_list dim max =
  let rec create_lst lst cnt =
    match lst with
    | _ when cnt >= dim-1 -> lst
    | lst -> create_lst (lst @ [(Random.int max) + 1]) (cnt + 1)
  in
  create_lst [] 0
;;

(* List.init solution *)
let rnd_list_fun dim max =
  List.init dim (fun _ -> (Random.int max)+1)
;;
